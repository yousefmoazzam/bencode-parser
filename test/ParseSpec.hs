module ParseSpec where

import BencodeParser (BencodeData (BInteger), parseInt)
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
        (parse parseInt "" "i1234e" `shouldParse` BInteger 1234)
