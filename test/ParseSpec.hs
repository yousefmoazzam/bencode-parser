module ParseSpec where

import BencodeParser (BencodeData (..), parseByteString, parseInt, parseList)
import qualified Data.ByteString as B
import Test.Hspec
import Test.Hspec.Megaparsec (shouldParse)
import Text.Megaparsec (parse)

spec :: Spec
spec =
  describe
    "Parsing tests"
    $ do
      it
        "Bencoded positive integer parses to integer variant with correct value"
        -- ASCII for `i1234e`
        (parse parseInt "" (B.pack [105, 49, 50, 51, 52, 101]) `shouldParse` BInteger 1234)

      it
        "Bencoded negative integer parses to integer variant with correct value"
        -- ASCII for `i-1234e`
        (parse parseInt "" (B.pack [105, 45, 49, 50, 51, 52, 101]) `shouldParse` BInteger (-1234))

      it
        "Bencoded bytestring parses to bytestring variant with correct value"
        $ do
          let input = B.pack ([53, 58] ++ hello)
           in -- ASCII for `5:hello`
              (parse parseByteString "" input `shouldParse` BByteString (B.pack hello))

      it
        "Bencoded list containing single bytestring parses to list variant with correct value"
        $ do
          let input = B.pack ([108, 53, 58] ++ hello ++ [101])
           in -- ASCII for `l5:helloe`
              parse parseList "" input `shouldParse` BList [BByteString $ B.pack hello]
  where
    hello = [104, 101, 108, 108, 111]
