{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module ChanWeb where

import Yesod
import ChanLib

data App = App

mkYesod "App" [parseRoutes|
/               HomeR  GET
/board/#String  BoardR GET
|]

instance Yesod App

getHomeR = defaultLayout [whamlet|<a href=@{BoardR "g"}>View Parsed|]

getBoardR :: String -> Handler Html
getBoardR b = defaultLayout $ do
    liftIO go >>= \x -> toWidget $ [hamlet|<p>#{x} |]
        where go = do g <- getBoard $ "http://a.4cdn.org/" ++ b ++ "/catalog.json"
                      case g of
                        (Right board) -> return $ show board
                        (Left s)      -> return s

run :: IO ()
run = warp 3000 App
