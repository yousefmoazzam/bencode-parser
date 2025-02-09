module BencodeParser (BencodeData (BInteger), parseInt) where

import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Byte (char)
import qualified Text.Megaparsec.Byte.Lexer as L

-- | Bencoded data types
data BencodeData
  = BInteger Int
  deriving (Eq, Show)

type Parser = Parsec Void ByteString

-- | Parse bencoded integer
parseInt :: Parser BencodeData
parseInt =
  BInteger
    <$> ( parseI
            *> L.signed (pure ()) L.decimal
            >>= \val ->
              parseE
                *> pure val
        )
  where
    parseI = void (char 105) -- ASCII for `i`
    parseE = void (char 101) -- ASCII for `e`
