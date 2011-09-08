{-# LANGUAGE OverloadedStrings #-}

{-|

This is where all the routes and handlers are defined for your site. The
'site' function combines everything together and is exported by this module.

-}

module Site
  ( site
  ) where

import           Control.Applicative
import           Data.Maybe
import qualified Data.Text.Encoding as T
import           Snap.Extension.Heist
import           Snap.Util.FileServe
import           Snap.Types
import           Text.Templating.Heist

import           Application
import Network.HTTP.Enumerator
import qualified Data.ByteString.Char8 as B
import Control.Monad.IO.Class
import Data.IORef
import Snap.Internal.Http.Types
import Blaze.ByteString.Builder
import Data.Enumerator
import Snap.Iteratee


------------------------------------------------------------------------------
-- | Renders the front page of the sample site.
--
-- The 'ifTop' is required to limit this to the top of a route.
-- Otherwise, the way the route table is currently set up, this action
-- would be given every request.
index :: Application ()
index = do
    murl <- getParam "url"
    case murl of
        Nothing -> finishWith emptyResponse {rspStatus=400, rspStatusReason="Bad request"}
        Just url -> do
            req <- getRequest
            let enum step = do
                    -- build new request
                    newReq' <- liftIO $ parseUrl $ B.unpack url
                    newReq <-
                        case (getHeader "content-length" req) of
                            (Just slen) -> liftIO $ do
                                -- set request body stream
                                let bodyRef = rqBody req
                                (SomeEnumerator enumBody') <- readIORef bodyRef
                                writeIORef bodyRef (SomeEnumerator enumEOF)
                                let enumReq = mapEnum toByteString fromByteString enumBody'
                                    len = read (B.unpack slen)
                                return newReq' { requestBody = RequestBodyEnum len enumReq }
                            Nothing -> return newReq'

                    -- run the new request with passed in continuation
                    liftIO $ withManager $ \m ->
                        run_ $ http newReq (\_ _ -> returnI step) m

            modifyResponse $ setResponseBody $ mapEnum toByteString fromByteString enum


------------------------------------------------------------------------------
-- | Renders the echo page.
echo :: Application ()
echo = do
    message <- decodedParam "stuff"
    heistLocal (bindString "message" (T.decodeUtf8 message)) $ render "echo"
  where
    decodedParam p = fromMaybe "" <$> getParam p


------------------------------------------------------------------------------
-- | The main entry point handler.
site :: Application ()
site = route [ ("/",            index)
             , ("/echo/:stuff", echo)
             ]
       <|> serveDirectory "resources/static"
