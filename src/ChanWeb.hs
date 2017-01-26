{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ChanWeb where

import Yesod
import ChanLib

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    liftIO go >>= \x -> toWidget $ [hamlet| <p>#{x} |]
        where go = do g <- getBoard "http://a.4cdn.org/g/catalog.json"
                      case g of
                        (Right board) -> return $ show board
                        (Left s)      -> return s

run :: IO ()
run = warp 3000 App
