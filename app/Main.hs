module Main where

import ChanLib

getG = getBoard "http://a.4cdn.org/g/catalog.json"

main :: IO ()
main = do g <- getG
          case g of
            (Right board) -> putStrLn $ foldl getComments "" $ getAllThreads board -- successfull download and parse
            (Left s)      -> putStrLn s  -- error
    where getComments cs t = case (tCom t) of
                               (Just com) -> cs ++ com ++ "\n"
                               Nothing    -> cs ++ "-----------------------" ++ "\n"
