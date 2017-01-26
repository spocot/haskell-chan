{-# LANGUAGE TemplateHaskell #-}

module ChanLib where

import Data.Char (toLower)
import System.IO
import Data.Maybe
import Network.HTTP
import Network.URI
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as BS

data Board = Board [Page] deriving Show

data Page = Page { threads :: [Thread] } deriving Show

data Thread = Thread { tNo          :: Int
                     , tSticky      :: Maybe Int
                     , tClosed      :: Maybe Int
                     , tNow         :: String
                     , tName        :: String
                     , tCom         :: Maybe String
                     , tFilename    :: String
                     , tExt         :: String
                     , tW           :: Int
                     , tH           :: Int
                     , tTim         :: Int
                     , tTime        :: Int
                     , tMD5         :: String
                     , tLastReplies :: Maybe [Post] } deriving Show

data Post = Post { pNo       :: Int
                 , pNow      :: String
                 , pName     :: String
                 , pCom      :: Maybe String
                 , pFilename :: Maybe String
                 , pExt      :: Maybe String
                 , pW        :: Maybe Int
                 , pH        :: Maybe Int
                 , pTim      :: Int
                 , pTime     :: Int
                 , pMD5      :: Maybe String } deriving Show

-- Generate JSON instances for Board and Page.
$(concat <$> mapM (deriveJSON defaultOptions) [''Board, ''Page])

-- Generate JSON instances for Thread and Post using more advanced parsing rules.
$(concat <$> mapM (deriveJSON defaultOptions { fieldLabelModifier = map toLower . drop 1}) [''Thread, ''Post])

{-|
    The 'parseBoard' function packs a given JSON String as a lazy ByteString and attempts to decode it using Aeson.
    Just Board on success.
    Nothing on parse failure.
-}
parseBoard :: String -> Maybe Board
parseBoard s = decode $ BS.pack s

{-|
    The 'uriToRequest' function creates a simple GET request from a given URI.
-}
uriToRequest :: URI -> Request String
uriToRequest s = Request { rqURI     = s
                         , rqMethod  = GET
                         , rqHeaders = []
                         , rqBody    = "" }

{-|
    The 'strToRequest' function attempts to create a request from a url as a String.
    Just (Request String) on success.
    Nothing on failure.
-}
strToRequest :: String -> Maybe (Request String)
strToRequest url = uriToRequest <$> parseURI url

{-|
    The 'downloadURL' function downloads a file from a URL.
    (Left errorMsg) if an error occurs,
    (Right doc) if success.
    It takes one argument, of type 'String'
-}
downloadURL :: String -> IO (Either String String)
downloadURL s = do case (strToRequest s) of
                     Nothing  -> return $ Left "URL parse error."
                     Just req -> do resp <- simpleHTTP req
                                    case resp of
                                      Left x  -> return $ Left ("Error connecting: " ++ show x)
                                      Right r -> case rspCode r of
                                                   (2,_,_) -> return $ Right (rspBody r)
                                                   _       -> return $ Left  (show r)
 
{-|
    The 'getBoard' function downloads and parses a chan board given a URL as a String.
    IO (Left String) on failure.
    IO (Right Board) on sucess.
-}
getBoard :: String -> IO (Either String Board)
getBoard url = do rsp <- downloadURL url
                  case rsp of
                    (Left err) -> return $ Left err
                    (Right r)  -> case (parseBoard r) of
                                    (Just b) -> return $ Right b
                                    _        -> return $ Left "JSON parse error."

getAllThreads :: Board -> [Thread]
getAllThreads (Board pages) = concatMap (threads) pages
