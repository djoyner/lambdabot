{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleContexts #-}
module Lambdabot.Util.Token (
     defaultUser
   , defaultPassword
   , defaultScope
   , scopePrefix
   , sendTokenRequest
   , parseTokenResponse
) where

import Lambdabot.Util.HTTP

import Control.Applicative ((<$>), empty)
import Control.Monad (liftM)
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Text.Printf

endpoint :: String
endpoint = "https://%s/RSTS/oauth2/token"

defaultUser :: String
defaultUser = "admin"

defaultPassword :: String
defaultPassword = "Admin123"

defaultScope :: String
defaultScope = scopePrefix ++ "local"

scopePrefix :: String
scopePrefix = "dell:sts:primaryproviderid:"

data TokenRequest = TokenRequest {
    tokenRequestUsername :: T.Text,
    tokenRequestPassword :: T.Text,
    tokenRequestScope :: T.Text
}

instance ToJSON TokenRequest where
    toJSON TokenRequest{..} = object
        [ "grant_type" .= T.pack "password"
        , "username" .= tokenRequestUsername
        , "password" .= tokenRequestPassword
        , "scope" .= tokenRequestScope
        ]

data TokenResponse = TokenResponse {
    tokenResponseAccessToken :: T.Text
}

instance FromJSON TokenResponse where
    parseJSON (Object v) = TokenResponse <$> v .: "access_token"
    parseJSON _ = empty

sendTokenRequest :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) => String -> String -> String -> String -> m (Response BL.ByteString)
sendTokenRequest addr user pass scope = sendRequest "POST" url (Just [contentTypeJson]) (Just body)
    where url = printf endpoint addr
          body = RequestBodyLBS $ encode $ TokenRequest (T.pack user) (T.pack pass) (T.pack scope)

parseTokenResponse :: Response (BL.ByteString) -> (Status, Maybe String)
parseTokenResponse resp
    | statusIsSuccessful status = (status, mAccessToken)
    | otherwise = (status, Nothing)
    where status = responseStatus resp
          body = responseBody resp
          mAccessToken = decode body >>= liftM (T.unpack . tokenResponseAccessToken)
