module BencodeParser (BencodeData (..), parseBencode) where

import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Map
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Byte (char)
import qualified Text.Megaparsec.Byte.Lexer as L

-- | Bencoded data types
data BencodeData
  = BInteger Int
  | BByteString ByteString
  | BList [BencodeData]
  | BDict (Map ByteString BencodeData)
  deriving (Eq, Show)

type Parser = Parsec Void ByteString

-- | Parse bencoded data
parseBencode :: Parser BencodeData
parseBencode = parseInt <|> parseByteString <|> parseList <|> parseDict

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
            *> Text.Megaparsec.some parseBencode
            >>= \strings ->
              parseE
                *> pure strings
        )
  where
    parseL = void (char 108) -- ASCII for `l`

parseDict :: Parser BencodeData
parseDict =
  BDict
    <$> ( parseD
            *> Text.Megaparsec.some parseDictPair
            >>= \pairs ->
              parseE
                *> pure (fromList pairs)
        )
  where
    parseD = void $ char 100

parseDictPair :: Parser (ByteString, BencodeData)
parseDictPair =
  parseByteString >>= \key ->
    parseBencode >>= \val ->
      case key of
        BByteString keyByteString -> pure (keyByteString, val)
        _ -> error "Expected bytestring variant for dict key"

-- | Matches ASCII code for `e` character
parseE :: Parser ()
parseE = void (char 101)
