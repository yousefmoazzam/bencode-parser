module Main where

import BencodeParser (parseBencode)
import qualified Data.ByteString
import System.Environment (getArgs)
import Text.Megaparsec (parse)

main :: IO ()
main =
  getArgs >>= \args ->
    case args of
      [] ->
        Data.ByteString.getContents
          >>= \contents -> print (parse parseBencode "" contents)
      _ -> putStrLn "Currently only supports input from stdin"
