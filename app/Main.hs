module Main where

import ChanLib

main :: IO ()
main = do b <- getBoard "http://a.4cdn.org/g/catalog.json"
          case b of
            (Right board) -> print board -- successfull download and parse
            (Left s)      -> putStrLn s  -- error