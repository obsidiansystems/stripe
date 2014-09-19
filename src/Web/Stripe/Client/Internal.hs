{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Web.Stripe.Client.Internal
    ( callAPI
    , runStripe
    , Stripe             (..)
    , StripeRequest      (..)
    , StripeError        (..)
    , StripeConfig       (..)
    , Method             (..)
    , module Web.Stripe.Client.Util
    ) where

import           Control.Applicative        ((<$>), (<*>))
import           Control.Exception          (SomeException, try)
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.Aeson                 (FromJSON, Value (Object),
                                             decodeStrict, parseJSON, (.:))
import           Data.ByteString            (ByteString)
import           Data.Maybe                 (fromJust, fromMaybe)
import           Data.Monoid                (mempty, (<>))
import           Data.Text                  (Text)
import           Network.Http.Client        (Connection, Method (..),
                                             baselineContextSSL, buildRequest,
                                             closeConnection, concatHandler,
                                             emptyBody, getStatusCode, http,
                                             inputStreamBody, openConnectionSSL,
                                             receiveResponse, sendRequest,
                                             setAuthorizationBasic,
                                             setContentType, setHeader)
import           OpenSSL                    (withOpenSSL)
import           Web.Stripe.Client.Error    (StripeError (..),
                                             StripeErrorHTTPCode (..),
                                             StripeErrorType (..))
import           Web.Stripe.Client.Types    (Stripe, StripeConfig (..),
                                             StripeRequest (..))
import           Web.Stripe.Client.Util     (paramsToByteString, toText, fromSeconds, (</>), getParams)

import qualified Data.ByteString            as S
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified System.IO.Streams          as Streams

------------------------------------------------------------------------------
-- | Main entry point for beginning a `Stripe` API request
runStripe
    :: FromJSON a
    => StripeConfig
    -> Stripe a
    -> IO (Either StripeError a)
runStripe = flip runReaderT

------------------------------------------------------------------------------
-- | API request to be issued
callAPI
    :: FromJSON a
    => StripeRequest
    -> Stripe a
callAPI request =
    ask >>= \config ->
        liftIO $ sendStripeRequest config request

------------------------------------------------------------------------------
-- | The guts of issuing an HTTP Request to a `Stripe` API endpoint
sendStripeRequest
    :: FromJSON a
    => StripeConfig
    -> StripeRequest
    -> IO (Either StripeError a)
sendStripeRequest
    StripeConfig {..}
    StripeRequest{..} =
        withOpenSSL $ do
          ctx <- baselineContextSSL
          result <- try (openConnectionSSL ctx "api.stripe.com" 443) :: IO (Either SomeException Connection)
          case result of
            Left  msg -> return $ Left $ StripeError ConnectionFailure (toText msg) Nothing Nothing Nothing
            Right con -> handleConnection con
  where
    handleConnection con = do
      let reqBody | method == GET = mempty
                  | otherwise     = paramsToByteString params
          reqURL  | method == GET = S.concat [ T.encodeUtf8 url
                                             , "?"
                                             , paramsToByteString params
                                             ]
                  | otherwise = T.encodeUtf8 url
      req <- buildRequest $ do
          http method $ "/v1/" <> reqURL
          setAuthorizationBasic secretKey mempty
          setContentType "application/x-www-form-urlencoded"
          setHeader "Stripe-Version" apiVersion
      body <- Streams.fromByteString reqBody
      sendRequest con req $ inputStreamBody body
      json <- receiveResponse con $
              \response inputStream ->
                  concatHandler response inputStream >>= \result -> do
                      print result
                      {- for debugging purposes -} 
                      handleStream response result
      closeConnection con
      return json
    parseFail = error "Parse failure"
    handleStream p x =
        return $ case getStatusCode p of
                   200 -> maybe parseFail Right (decodeStrict x)
                   code | code >= 400 ->
                     do let json = fromMaybe parseFail (decodeStrict x :: Maybe StripeError)
                        Left $ case code of
                                 400 -> json { errorHTTP = Just BadRequest }
                                 401 -> json { errorHTTP = Just UnAuthorized }
                                 402 -> json { errorHTTP = Just RequestFailed }
                                 404 -> json { errorHTTP = Just NotFound }
                                 500 -> json { errorHTTP = Just StripeServerError }
                                 502 -> json { errorHTTP = Just StripeServerError }
                                 503 -> json { errorHTTP = Just StripeServerError }
                                 504 -> json { errorHTTP = Just StripeServerError }
                                 _   -> json { errorHTTP = Just UnknownHTTPCode }
