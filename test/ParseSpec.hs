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
        "Parse bencoded positive integer"
        -- ASCII for `i1234e`
        (parse parseInt "" (B.pack $ [105] ++ num ++ [101]) `shouldParse` BInteger 1234)

      it
        "Parse bencoded negative integer"
        -- ASCII for `i-1234e`
        (parse parseInt "" (B.pack $ [105, 45] ++ num ++ [101]) `shouldParse` BInteger (-1234))

      it
        "Parse bencoded bytestring"
        $ do
          let input = B.pack ([53, 58] ++ hello)
           in -- ASCII for `5:hello`
              (parse parseByteString "" input `shouldParse` BByteString (B.pack hello))

      it
        "Parse bencoded list with single bytestring element"
        $ do
          let input = B.pack ([108, 53, 58] ++ hello ++ [101])
           in -- ASCII for `l5:helloe`
              parse parseList "" input `shouldParse` BList [BByteString $ B.pack hello]

      it
        "Parse bencoded list with multiple bytestring elements"
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

      it
        "Parse bencoded dict containing multiple keys with bytestring values"
        $ do
          let key1 = [53, 58] ++ hello
              value1 = [51, 58] ++ foo
              key2 = [55, 58] ++ goodbye
              bar = [98, 97, 114]
              value2 = [51, 58] ++ bar
              input = B.pack $ [100] ++ key1 ++ value1 ++ key2 ++ value2 ++ [101]
              expectedMap = fromList [(B.pack hello, BByteString $ B.pack foo), (B.pack goodbye, BByteString $ B.pack bar)]
           in -- ASCII for `d5:hello3:foo7:goodbye3:bare`
              parse parseDict "" input `shouldParse` BDict expectedMap

      it
        "Parse bencoded dict containing single key and integer value"
        $ do
          let key = [53, 58] ++ hello
              value = [105] ++ num ++ [101]
              input = B.pack $ [100] ++ key ++ value ++ [101]
              expectedMap = fromList [(B.pack hello, BInteger 1234)]
           in -- ASCII for `d5:helloi1234ee`
              parse parseDict "" input `shouldParse` BDict expectedMap

      it
        "Parse bencoded dict containing single key and list value"
        $ do
          let key = [53, 58] ++ hello
              item1 = [55, 58] ++ goodbye
              item2 = [51, 58] ++ foo
              input = B.pack $ [100] ++ key ++ [108] ++ item1 ++ item2 ++ [101, 101]
              expectedList = BList [BByteString $ B.pack goodbye, BByteString $ B.pack foo]
              expectedMap = fromList [(B.pack hello, expectedList)]
           in -- ASCII for `d5:hellol7:goodbye3:fooee`
              parse parseDict "" input `shouldParse` BDict expectedMap

      it
        "Parse nested bencoded dicts"
        $ do
          let outer = [111, 117, 116, 101, 114]
              outerKey = [53, 58] ++ outer
              innerKey1 = [53, 58] ++ hello
              innerKey2 = [51, 58] ++ foo
              innerVal1 = [55, 58] ++ goodbye
              innerVal2 = [105] ++ num ++ [101]
              pair1 = innerKey1 ++ innerVal1
              pair2 = innerKey2 ++ innerVal2
              input = B.pack $ [100] ++ outerKey ++ [100] ++ pair1 ++ pair2 ++ [101, 101]
              expectedInnerMap = fromList [(B.pack hello, BByteString $ B.pack goodbye), (B.pack foo, BInteger 1234)]
              expectedMap = fromList [(B.pack outer, BDict expectedInnerMap)]
           in -- ASCII for `d5:outerd5:hello7:goodbye3:fooi1234eee`
              parse parseDict "" input `shouldParse` BDict expectedMap

      it
        "Parser bencoded list containing dict"
        $ do
          let key = [53, 58] ++ hello
              value = [55, 58] ++ goodbye
              input = B.pack $ [108, 100] ++ key ++ value ++ [101, 101]
              expectedMap = fromList [(B.pack hello, BByteString $ B.pack goodbye)]
           in -- ASCII for `ld5:hello7:goodbyeee`
              parse parseList "" input `shouldParse` BList [BDict expectedMap]
  where
    num = [49, 50, 51, 52]
    hello = [104, 101, 108, 108, 111]
    foo = [102, 111, 111]
    goodbye = [103, 111, 111, 100, 98, 121, 101]
