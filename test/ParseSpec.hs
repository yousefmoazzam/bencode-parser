module ParseSpec where

import BencodeParser (BencodeData (..), parseByteString, parseDict, parseInt, parseList)
import qualified Data.ByteString as B
import Data.Map
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
        (parse parseInt "" (B.pack $ [105] ++ num ++ [101]) `shouldParse` BInteger 1234)

      it
        "Bencoded negative integer parses to integer variant with correct value"
        -- ASCII for `i-1234e`
        (parse parseInt "" (B.pack $ [105, 45] ++ num ++ [101]) `shouldParse` BInteger (-1234))

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

      it
        "Bencoded list containing multiple bytestrings parses to list variant with correct value"
        $ do
          let input = B.pack ([108, 53, 58] ++ hello ++ [51, 58] ++ foo ++ [101])
              expectedList = [BByteString $ B.pack hello, BByteString $ B.pack foo]
           in -- ASCII for `l5:hello3:fooe`
              parse parseList "" input `shouldParse` BList expectedList

      it
        "Parser bencoded list containing integer and bytestring"
        $ do
          let input = B.pack ([108, 105] ++ num ++ [101] ++ [53, 58] ++ hello ++ [101])
              expectedList = [BInteger 1234, BByteString $ B.pack hello]
           in -- ASCII for `li1234e5:helloe`
              parse parseList "" input `shouldParse` BList expectedList

      it
        "Parser nested bencoded lists"
        $ do
          let bencodedHello = [53, 58] ++ hello
              bencodedFoo = [51, 58] ++ foo
              bencodedInt = [105] ++ num ++ [101]
              input = B.pack ([108] ++ bencodedInt ++ [108] ++ bencodedHello ++ bencodedFoo ++ [101, 101])
              expectedList = [BInteger 1234, BList [BByteString $ B.pack hello, BByteString $ B.pack foo]]
           in -- ASCII for `li1234el5:hello3:fooee`
              parse parseList "" input `shouldParse` BList expectedList

      it
        "Parse bencoded dict containing single key and bytestring value"
        $ do
          let bencodedKey = [53, 58] ++ hello
              bencodedValue = [51, 58] ++ foo
              input = B.pack $ [100] ++ bencodedKey ++ bencodedValue ++ [101]
              expectedMap = fromList [(B.pack hello, BByteString $ B.pack foo)]
           in -- ASCII for `d5:hello3:fooe`
              parse parseDict "" input `shouldParse` BDict expectedMap
  where
    num = [49, 50, 51, 52]
    hello = [104, 101, 108, 108, 111]
    foo = [102, 111, 111]
