module ParseSpec where

import BencodeParser (BencodeData (BInteger), parseInt)
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
