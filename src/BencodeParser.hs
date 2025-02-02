module BencodeParser (BencodeData (BInteger), parseInt) where

import Control.Monad (void)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import qualified Text.Megaparsec.Char.Lexer as L

-- | Bencoded data types
data BencodeData
  = BInteger Int
  deriving (Eq, Show)

type Parser = Parsec Void String

-- | Parse bencoded integer
parseInt :: Parser BencodeData
parseInt =
  BInteger
    <$> ( parseI
            *> L.decimal
            >>= \val ->
              parseE
                *> pure val
        )
  where
    parseI = void (char 'i')
    parseE = void (char 'e')
