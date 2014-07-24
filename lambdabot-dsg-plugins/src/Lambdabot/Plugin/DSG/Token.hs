{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleContexts #-}
module Lambdabot.Plugin.DSG.Token (tokenPlugin) where

import Lambdabot.Plugin
import Lambdabot.Util.Token

import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Text.Printf

type Token = ModuleT () LB

tokenPlugin :: Module ()
tokenPlugin = newModule
    { moduleCmds = return
        [ (command "token")
            { help = say "token address user password provider.  Obtain an OAuth2 token for a user."
            , process = tokenCmd
            }
        ]
    }

tokenCmd :: String -> Cmd Token ()
tokenCmd str
    | nargs >= 3 = do
        resp <- liftIO $ sendTokenRequest addr user pass scope
        sayTokenResponse resp
    | otherwise = say "usage: @token <address> <user> <password> [provider]"
    where args = words str
          nargs = length args
          addr = args !! 0
          user = args !! 1
          pass = args !! 2
          scope = if (nargs > 3) then scopePrefix ++ (args !! 3) else defaultScope

sayTokenResponse :: Response (BL.ByteString) -> Cmd Token ()
sayTokenResponse resp
    | statusIsSuccessful status = say $ maybe (statusLine ++ " (with invalid body)") id mAccessToken
    | otherwise = say statusLine
    where (status, mAccessToken) = parseTokenResponse resp
          statusLine = printf "%d %s" (statusCode status) (T.unpack $ decodeUtf8 $ statusMessage status)
