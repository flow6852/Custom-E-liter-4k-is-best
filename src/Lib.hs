{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module Lib where

import System.IO
import Control.Concurrent
import Control.Exception
import Data.Text
import Data.Text.IO 
import Data.Text.Encoding
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Web.Authenticate.OAuth
import Data.ByteString.Lazy.Internal
import Control.Monad.IO.Class

--post TL parser
data PostTL = PostTL { ptl_id_str :: Text} deriving (Show)
$(deriveJSON defaultOptions {fieldLabelModifier = Prelude.drop 4 } ''PostTL)

tweet :: Text -> Text -> [String] -> IO (Either String PostTL)
tweet tw twid botconf = do
 responce <- do
  req     <- parseRequest $ "https://api.twitter.com/1.1/statuses/update.json" ++ if Data.Text.null twid then "" 
                                                                                  else "?in_reply_to_status_id=" ++ unpack twid
  let postReq = urlEncodedBody [("status", encodeUtf8 tw)] req
  httpManager postReq botconf
 return $ eitherDecode $ responseBody responce

httpManager :: Request -> [String] ->  IO(Response Data.ByteString.Lazy.Internal.ByteString)
httpManager req botconf = do
 (myOAuth, myCredential) <- botuser botconf
 signedReq <- signOAuth myOAuth myCredential req
 manager <- newManager tlsManagerSettings
 Network.HTTP.Conduit.httpLbs signedReq manager

botuser :: [String] -> IO(OAuth,Credential)
botuser botsparameter = do
 let  myOAuth      = newOAuth { oauthServerName     = "api.twitter.com"
                              , oauthConsumerKey    = C.pack(Prelude.head botsparameter)
                              , oauthConsumerSecret = C.pack(botsparameter !! 1)
  }
      myCredential = newCredential (C.pack(botsparameter !! 2)) (C.pack(botsparameter !! 3))
 return (myOAuth, myCredential)

