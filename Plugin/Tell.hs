{- | Leave a message with lambdabot, the faithful secretary.

> 17:11 < davidhouse> @tell dmhouse foo
> 17:11 < hsbot> Consider it noted
> 17:11 < davidhouse> @tell dmhouse bar
> 17:11 < hsbot> Consider it noted
> 17:11 < dmhouse> hello!
> 17:11 < hsbot> dmhouse: You have 2 new messages. '/msg hsbot @messages' to read them.
> 17:11 < dmhouse> Notice how I'm speaking again, and hsbot isn't buzzing me more than that one time.
> 17:12 < dmhouse> It'll buzz me after a day's worth of not checking my messages.
> 17:12 < dmhouse> If I want to check them in the intermittent period, I can either send a /msg, or:
> 17:12 < dmhouse> @messages?
> 17:12 < hsbot> You have 2 messages
> 17:12 < dmhouse> Let's check them, shall we?
>
> [In a /msg to hsbot]
> 17:12 <hsbot> davidhouse said less than a minute ago: foo
> 17:12 <hsbot> davidhouse said less than a minute ago: bar
>
> [Back in the channel
> 17:12 < dmhouse> You needn't use a /msg, however. If you're not going to annoy the channel by printing 20 of
>                  your messages, feel free to just type '@messages' in the channel.
> 17:12 < davidhouse> @tell dmhouse foobar
> 17:12 < hsbot> Consider it noted
> 17:12 < davidhouse> @ask dmhouse barfoo
> 17:12 < hsbot> Consider it noted
> 17:12 < davidhouse> You can see there @ask. It's just a synonym for @tell, but it prints "foo asked X ago M",
>                     which is more natural. E.g. '@ask dons whether he's applied my latest patch yet?'
> 17:13 < dmhouse> For the admins, a useful little debugging tool is @print-notices.
> 17:13 < hsbot> dmhouse: You have 2 new messages. '/msg hsbot @messages' to read them.
> 17:14 < dmhouse> Notice that hsbot pinged me there, even though it's less than a day since I last checked my
>                  messages, because there have been some new ones posted.
> 17:14 < dmhouse> @print-notices
> 17:14 < hsbot> {"dmhouse":=(Just Thu Jun  8 17:13:46 BST 2006,[Note {noteSender = "davidhouse", noteContents =
>                "foobar", noteTime = Thu Jun  8 17:12:50 BST 2006, noteType = Tell},Note {noteSender = 
                 "davidhouse", noteContents = "barfoo", noteTime = Thu Jun  8 17:12:55 BST 2006, noteType = Ask}])}
> 17:15 < dmhouse> There you can see the two notes. The internal state is a map from recipient nicks to a pair of
>                  (when we last buzzed them about having messages, a list of the notes they've got stacked up).
> 17:16 < dmhouse> Finally, if you don't want to bother checking your messages, then the following command will
>                  likely be useful.
> 17:16 < dmhouse> @clear-messages
> 17:16 < hsbot> Messages cleared.
> 17:16 < dmhouse> That's all, folks!
> 17:17 < dmhouse> Any comments, queries or complaints to dmhouse@gmail.com. The source should be fairly readable, so
>                  hack away!
-}

module Plugin.Tell where

import Control.Arrow (first)
import qualified Data.Map as M
import System.Time
import Text.Printf (printf)

import Message
import Plugin

type Nick        = String
-- | Was it @tell or @ask that was the original command?
data NoteType    = Tell | Ask deriving (Show, Eq)
-- | The Note datatype. Fields self-explanatory.
data Note        = Note { noteSender   :: String, 
                          noteContents :: String, 
                          noteTime     :: ClockTime,
                          noteType     :: NoteType }
                   deriving (Eq, Show{-, Read-})
-- | The state. A map of (times we last told this nick they've got messages, the
--   messages themselves)
type NoticeBoard = M.Map Nick (Maybe ClockTime, [Note])
-- | A nicer synonym for the Tell monad.
type Telling a   = ModuleT NoticeBoard LB a 

PLUGIN Tell

instance Module TellModule NoticeBoard where
    moduleCmds      _ = ["tell", "ask", "messages", "messages?", "clear-messages"]
    modulePrivs     _ = ["print-notices"]
    moduleHelp _      = fromJust . flip lookup help
    moduleDefState  _ = return M.empty
    -- moduleSerialize _ = Just mapSerial -- ClockTime doesn't instantiate Read

    -- | Debug output the NoticeBoard
    process _ _ _ "print-notices" _ = liftM ((:[]) . show) readMS

    -- | Clear a user's notes
    process _ msg _ "clear-messages" _ = 
      clearMessages (nick msg) >> return ["Messages cleared."]

    -- | Check whether a user has any messages
    process _ msg _ "messages?" _  = do
      let sender = nick msg
      ms <- getMessages sender
      case ms of
        Just _ -> doRemind sender
        Nothing   -> return ["Sorry, no messages today."]

    -- | Write down a note
    process _ msg _ "tell" args = doTell args msg Tell

    -- | Really just a synonym for @tell, but phrases it as a question instead.
    process _ msg _ "ask" args = doTell args msg Ask

    -- | Give a user their messages
    process _ msg _ "messages" _ =
      do msgs <- getMessages $ nick msg
         let res = fromMaybe ["You don't have any new messages."] msgs
         clearMessages (nick msg)
         return res

    -- | Hook onto contextual. Grab nicks of incoming messages, and tell them
    --   if they have any messages, if it's less than a day since we last did so.
    contextual _ msg _ _ = do 
      let sender = nick msg
      remp <- needToRemind sender
      if remp
         then doRemind sender
         else return []

-- | Lookup table for documentation
help :: [(String, String)]
help = [("tell",
         "tell <nick> <message>. When <nick> shows activity, tell them " ++
           "<message>."),
        ("ask",
         "ask <nick> <message>. When <nick> shows activity, ask them " ++
           "<message>."),
        ("messages",
         "messages. Check your messages."),
        ("messages?",
         "messages?. Tells you whether you have any messages"),
        ("clear-messages",
         "clear-messages. Clears your messages."),
        ("print-notices", "Print the current map of notes.") ]

-- | Take a note and the current time, then display it
showNote :: ClockTime -> Note -> String
showNote time note = res
    where diff         = time `diffClockTimes` noteTime note
          diff'        = normalizeTimeDiff diff
          mkAgo td msg = if td > 0 then Just (show td ++ msg) else Nothing
          agos         = [ mkAgo (tdYear  diff') " years",
                           mkAgo (tdMonth diff') " months",
                           mkAgo (tdDay   diff') " days",
                           mkAgo (tdHour  diff') " hours",
                           mkAgo (tdMin   diff') " minutes" ]
          agos'        = catMaybes agos
          ago          = if null agos'
                           then "less than a minute"
                           else concatList agos'
          action       = case noteType note of Tell -> "said"; Ask -> "asked"
          res          = printf "%s %s %s ago: %s"
                           (noteSender note) action ago (noteContents note)

-- | Is it less than a day since we last reminded this nick they've got messages?
needToRemind :: Nick -> Telling Bool
needToRemind n = do
  st  <- readMS
  now <- io getClockTime
  return $ case M.lookup n st of
             Just (Just lastTime, _) -> 
               let diff = now `diffClockTimes` lastTime
               in diff > noTimeDiff { tdDay = 1 }
             Just (Nothing,       _) -> True
             Nothing                 -> True

-- | Add a note to the NoticeBoard
writeDown :: Nick -> Nick -> String -> NoteType -> Telling ()
writeDown to from what ntype = do 
  time <- io getClockTime
  let note = Note { noteSender   = from, 
                    noteContents = what, 
                    noteTime     = time,
                    noteType     = ntype }
  modifyMS (M.insertWith (\_ (_, ns) -> (Nothing, ns ++ [note]))
                         to (Nothing, [note]))

-- | Return a user's notes, or Nothing if they don't have any
getMessages :: Nick -> Telling (Maybe [String])
getMessages n = do 
  st   <- readMS
  time <- io getClockTime
  case M.lookup n st of
    Just (_, msgs) -> do
      -- update the last time we told this person they had messages
      writeMS $ M.insert n (Just time, msgs) st
      return . Just $ map (showNote time) msgs
    Nothing -> return Nothing

-- | Clear a user's messages.
clearMessages :: Nick -> Telling ()
clearMessages n = modifyMS (M.delete n)

-- * Handlers
--

-- | Execute a @tell or @ask command.
doTell :: Message m => String -> m -> NoteType -> Telling [String]
doTell args msg ntype = do 
  let args'     = words args
      recipient = head args'
      sender    = nick msg
      rest      = unwords $ tail args'
      res | sender    == recipient   = Left "You can tell yourself!"
          | recipient == name config = Left "Nice try ;)"
          | otherwise                = Right "Consider it noted."
  when (isRight res) (writeDown recipient sender rest ntype)
  return [unEither res]

-- | Remind a user that they have messages.
doRemind :: Nick -> Telling [String]
doRemind sender = do 
  ms  <- getMessages sender
  now <- io getClockTime
  modifyMS (M.update (Just . first (const $ Just now)) sender)
  return $ case ms of
             Just msgs -> 
               let (messages, pronoun) = 
                     if length msgs > 1
                       then ("messages", "them") else ("message", "it")
               in [printf "%s: You have %d new %s. '/msg %s @messages' to read %s."
                          sender (length msgs) messages (name config) pronoun
                   :: String]
             Nothing -> []