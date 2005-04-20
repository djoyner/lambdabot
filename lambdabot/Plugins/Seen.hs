--
-- | Keep track of IRC users.
--
module Plugins.Seen (theModule) where

import IRC
import Util (mapSerializer, lowerCaseString, firstWord, listToStr, debugStrLn)
import Config
import qualified Map as M

import Data.List ((\\),nub)

import Control.Monad.Trans (liftIO, MonadIO)
import Control.Arrow (first)

import System.Time (TimeDiff(..), noTimeDiff)
import qualified System.Time as T 
  (ClockTime(..), getClockTime, diffClockTimes, addToClockTime)

------------------------------------------------------------------------

------- Time compatibility layer (maybe move to its own module?) -------

-- Wrapping ClockTime (which doesn't provide a Read instance!) seems 
-- easier than talking care of the serialization of UserStatus ourselves.
newtype ClockTime = ClockTime (T.ClockTime)

instance Show ClockTime where
  showsPrec p (ClockTime (T.TOD x y)) = showsPrec p (x,y)

instance Read ClockTime where
  readsPrec p = map (first $ ClockTime . uncurry T.TOD) . readsPrec p

getClockTime :: IO ClockTime
getClockTime = ClockTime `fmap` T.getClockTime

diffClockTimes :: ClockTime -> ClockTime -> TimeDiff
diffClockTimes (ClockTime ct1) (ClockTime ct2) = T.diffClockTimes ct1 ct2

addToClockTime :: TimeDiff -> ClockTime -> ClockTime
addToClockTime td (ClockTime ct) = ClockTime $ T.addToClockTime td ct

------------------------------------------------------------------------

newtype SeenModule = SeenModule ()

theModule :: MODULE
theModule = MODULE $ SeenModule ()

-- | The type of channels
type Channel = String
-- | The type of nicknames
type Nick = String

-- | We last heard the user speak at ClockTime; since then we have missed
--   TimeDiff of him because we were absent.
type SpokeLast = Maybe (ClockTime, TimeDiff)

-- | 'UserStatus' keeps track of the status of a given Nick name.
data UserStatus
        = Present SpokeLast [Channel]  -- ^ Records when the nick last
                                       --   spoke and that the nick is
                                       --   currently in [Channel]
        | NotPresent ClockTime Channel -- ^ The nick is not present and was
                                       --   last seen at ClockTime in Channel
        | WasPresent ClockTime SpokeLast Channel -- ^ The bot parted a channel 
                                                 -- where the user was. The 
                                                 -- Clocktime records the time 
                                                 -- and Channel the channel
                                                 -- this happened in.
        | NewNick Nick                 -- ^ The user changed nick to something
                                       --   new.
    deriving (Show, Read)

type SeenState = M.Map Nick UserStatus
type Seen m a = ModuleT SeenState m a

instance Module SeenModule SeenState where
    moduleHelp _ _      = return "Report if a user has been seen by the bot"
    moduleCmds _        = return ["seen"]
    moduleDefState _    = return M.empty
    moduleSerialize _   = Just mapSerializer
    moduleInit _
      = do ircSignalConnect "JOIN"    joinCB
           ircSignalConnect "PART"    partCB
           ircSignalConnect "QUIT"    quitCB
           ircSignalConnect "NICK"    nickCB
           ircSignalConnect "353"     joinChanCB
           ircSignalConnect "PRIVMSG" msgCB

    process m msg target cmd rest =
      do seenFM <- readMS
         now <- liftIO getClockTime
         let myname = lowerCaseString (name config)
             nick' = firstWord rest
             you   = nick' == ircNick msg
             nick  = if you then "you" else nick'
             lcnick = lowerCaseString nick
             ircMessage = ircPrivmsg target . concat
             clockDifference = timeDiffPretty . diffClockTimes now
             -- I guess the only way out of this spagetty hell are 
             -- printf-style responses.
             nickPresent mct cs =
               ircPrivmsg target $ concat [
                 if you then "You are" else nick ++ " is in ", 
                 listToStr "and" cs, ".",
                 case mct of
                   Nothing -> 
                     concat [" I don't know when ", nick, " last spoke."]
                   Just (ct,missed)
                     |  missed == noTimeDiff
                     -> concat [" Last spoke ", lastSpoke, "."]
                     |  otherwise
                     -> concat [" I last heard ", nick, " speak ", when',
                                " but I have missed ", timeDiffPretty missed, 
                                " since then."]
                     where 
                       lastSpoke | "" <- when' = "just now"
                                 | otherwise   = when' ++ "ago"
                       when' = clockDifference ct
               ]
             nickNotPresent ct chan =
               ircMessage ["I saw ", nick, " leaving ", chan, " ",
                           clockDifference ct, "ago."]
             nickWasPresent ct chan =
               ircMessage ["Last time I saw ", nick, "was when I left ",
                           chan , " ", clockDifference ct, " ago."]
             nickIsNew newnick =
               do let findFunc str =
                        case M.lookup (lowerCaseString str) seenFM of
                          Just (NewNick str') -> findFunc str'
                          Just _              -> str
                          Nothing             -> error "SeenModule.nickIsNew: Nothing"
                      us = findFunc newnick
                  ircMessage [if you then "You have" else nick++"has", 
                              " changed nick to ", us, "."]
                  process m msg target cmd us
         if lcnick == myname
            then ircPrivmsg target "Yes, I'm here."
            else case M.lookup lcnick seenFM of
                  Just (Present mct cs) -> nickPresent mct cs
                  Just (NotPresent ct chan) -> nickNotPresent ct chan
                  Just (WasPresent ct _ chan) -> nickWasPresent ct chan
                  Just (NewNick newnick) -> nickIsNew newnick
                  _ -> ircPrivmsg target $ "I haven't seen " ++ nick ++ "."

-- | Callback for when somebody joins. If it is not the bot that joins, record
--   that we have a new user in our state tree and that we have never seen the
--   user speaking.
joinCB :: IRCMessage -> Seen IRC () -- when somebody joins
joinCB msg = withSeenFM msg $ \fm _ct myname nick ->
  if nick /= myname
     then let newInfo = Present Nothing (ircChans msg)
          in  Left $ M.insertWith (updateJ Nothing) nick newInfo fm
     else Left fm

partCB :: IRCMessage -> Seen IRC () -- when somebody parts
partCB msg = withSeenFM msg $ \fm ct myname nick ->
  let botPart cs us =
        case us of
          Present mct xs ->
            case xs \\ cs of
              [] -> WasPresent ct mct (listToStr "and" cs)
              ys -> Present mct ys
          _ -> us
  in if nick == myname
       then Left $ M.mapWithKey (const (botPart $ ircChans msg)) fm
       else case M.lookup nick fm of
              Just (Present mct xs) ->
                case xs \\ (ircChans msg) of
                  [] -> Left $ M.insert nick
                                        (NotPresent ct (listToStr "and" xs))
                                        fm
                  ys -> Left $ M.insert nick
                                        (Present mct ys)
                                        fm
              _ -> Right "SeenModule> someone who isn't known parted"

quitCB :: IRCMessage -> Seen IRC () -- when somebody quits
quitCB msg = withSeenFM msg $ \fm ct _myname nick ->
  case M.lookup nick fm of
    Just (Present _ct xs) -> Left $ M.insert nick (NotPresent ct (head xs)) fm
    _ -> Right "SeenModule> someone who isn't known has quit"

nickCB :: IRCMessage -> Seen IRC () -- when somebody changes his/her name
nickCB msg = withSeenFM msg $ \fm _ct _myname nick ->
  let newnick = drop 1 $ head (msgParams msg)
      lcnewnick = lowerCaseString newnick
  in case M.lookup nick fm of
       Just (Present mct xs) ->
         let fm' = M.insert nick (NewNick newnick) fm
         in Left $ M.insert lcnewnick (Present mct xs) fm'
       _ -> Right "SeenModule> someone who isn't here changed nick"

-- use IRC.ircChans?
joinChanCB :: IRCMessage -> Seen IRC () -- when the bot join a channel
joinChanCB msg = withSeenFM msg $ \fm now _myname _nick ->
  let l = msgParams msg
      chan = l !! 2
      chanUsers = words (drop 1 (l !! 3)) -- remove ':'
      insertNick fm' u = M.insertWith (updateJ $ Just now)
                                      (lowerCaseString $ unUserMode u)
                                      (Present Nothing [chan])
                                      fm'
      in Left $ foldl insertNick fm chanUsers

-- when somebody speaks, update their clocktime
msgCB :: IRCMessage -> Seen IRC ()
msgCB msg = withSeenFM msg $ \fm ct _myname nick ->
  case M.lookup nick fm of
    Just (Present mct xs) -> Left $ 
      M.insert nick (Present (Just (ct, maybe noTimeDiff snd mct)) xs) fm
    _ -> Right "SeenModule> someone who isn't here msg us"

-- misc. functions
unUserMode :: Nick -> Nick
unUserMode nick = dropWhile (`elem` "@+") nick


withSeenFM :: IRCMessage
              -> (SeenState -> ClockTime -> String -> Nick
                  -> Either SeenState String)
              -> Seen IRC ()
withSeenFM msg f = do let nick = (lowerCaseString . unUserMode)
                                         (ircNick msg)
                      state <- readMS
                      ct <- liftIO getClockTime
                      let myname = (lowerCaseString . name) config
                      case f state ct myname nick of
                        Left newstate -> writeMS newstate
                        Right err -> debugStrLn err

-- | Update the user status. Invariant: The first argument (i.e. the second
--   argument of updateJ' is always of the Form @Preset Nothing channels@.
--
-- TODO; Refactor
updateJ :: Maybe ClockTime -> UserStatus -> UserStatus -> UserStatus
updateJ iJoined = flip updateJ' where
  --             OLD            NEW
  updateJ' (Present ct cs) (Present _ct c) = Present ct $ nub (c ++ cs)
  updateJ' (WasPresent lastSeen (Just (lastSpoke, missed)) channel) (Present _ cs)
    | channel `elem` cs, Just now <- iJoined 
    --                 newMissed
    -- |---------------------------------------|
    -- |-------------------|                   |
    --        missed    lastSeen              now
    = let newMissed = addToClockTime missed now `diffClockTimes` lastSeen
      in  Present (Just (lastSpoke, newMissed)) cs
  updateJ' _x y@(Present _ct _cs) = y
  updateJ' _ _ = error "after suitable refactoring, this case doesn't need to be caught anymore"


-- annoying
timeDiffPretty :: TimeDiff -> String
timeDiffPretty td =
  let secs = tdSec td
      mins = secs `div` 60
      hours = mins `div` 60
      days = hours `div` 24
      months = days `div` 28
      years = months `div` 12
  in concat [prettyP years "year",
             prettyP (months `mod` 12) "month",
             prettyP (days `mod` 28) "day",
             prettyP (hours `mod` 24) "hour",
             prettyP (mins `mod` 60) "minute",
             prettyP (secs `mod` 60) "second"]
              where prettyP i str
                     | i == 0    = ""
                     | i == 1    = "1 " ++ str ++ " " 
                     | otherwise = show i ++ " " ++ str ++ "s "
