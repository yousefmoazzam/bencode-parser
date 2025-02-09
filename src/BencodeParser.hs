module BencodeParser (BencodeData (..), parseInt, parseByteString, parseList) where

import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Byte (char)
import qualified Text.Megaparsec.Byte.Lexer as L

-- | Bencoded data types
data BencodeData
  = BInteger Int
  | BByteString ByteString
  | BList [BencodeData]
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

parseByteString :: Parser BencodeData
parseByteString =
  BByteString
    <$> ( L.decimal >>= \len ->
            parseColon
              *> Text.Megaparsec.takeP Nothing len
              >>= \str ->
                pure str
        )
  where
    parseColon = void (char 58) -- ASCII for `:`

parseList :: Parser BencodeData
parseList =
  BList
    <$> ( parseL
            *> Text.Megaparsec.some parseByteString
            >>= \strings ->
              parseE
                *> pure strings
        )
  where
    parseL = void (char 108) -- ASCII for `l`

-- | Matches ASCII code for `e` character
parseE :: Parser ()
parseE = void (char 101)
