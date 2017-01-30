module Main where

import           Data.Aeson                 (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Obj                        (toJSON)
import           System.Environment         (getArgs)

main :: IO ()
main = do
    [file] <- getArgs
    result <- toJSON file
    case result of
        Right json -> LBS.writeFile "/tmp/model.json" $ encode json
        Left err   -> putStrLn err
