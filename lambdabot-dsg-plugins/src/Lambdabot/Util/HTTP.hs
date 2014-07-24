{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Lambdabot.Util.HTTP (
    contentTypeJson
  , sendRequest
) where

import Control.Exception.Base (SomeException)
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy.Char8 as BL
import Network.Connection
import Network.HTTP.Client.TLS
import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status

tlsSettings :: TLSSettings
tlsSettings = TLSSettingsSimple {
    settingDisableCertificateValidation = True,
    settingDisableSession = False,
    settingUseServerName = False
}

settings :: ManagerSettings
settings = mkManagerSettings tlsSettings Nothing

contentTypeJson :: Header
contentTypeJson = (hContentType, "application/json")

sendRequest :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) => Method -> String -> Maybe RequestHeaders -> Maybe RequestBody -> m (Response BL.ByteString)
sendRequest meth url mHeaders mBody = do
    req <- parseUrl url
    runResourceT $ do
      withManagerSettings settings $ \mgr -> do
        httpLbs (req { method = meth, requestHeaders = maybe (requestHeaders req) id mHeaders, requestBody = maybe (requestBody req) id mBody, checkStatus = ignoreStatus }) mgr

ignoreStatus :: Status -> ResponseHeaders -> CookieJar -> Maybe SomeException
ignoreStatus _ _ _ = Nothing
