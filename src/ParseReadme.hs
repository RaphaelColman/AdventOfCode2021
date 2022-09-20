{-# LANGUAGE ScopedTypeVariables #-}

module ParseReadme where
import           Control.Applicative.Combinators (choice,
                                                  manyTill, manyTill_, some,
                                                  (<|>), many)
import           Control.Exception               (SomeException)
import           Data.Data                       (Data (toConstr), Typeable)
import           Data.Foldable                   (Foldable (foldr'))
import           Data.Function                   (on)
import           Data.List                       (groupBy)
import qualified Data.Map                        as M
import qualified Data.Sequence                   as S
import           Text.Parser.LookAhead           (LookAheadParsing (lookAhead))
import           Text.Trifecta                   (CharParsing (anyChar, char, string),
                                                  Parser, Parsing (eof),
                                                  TokenParsing (someSpace),
                                                  charLiteral, newline,
                                                  parseFromFile, parseString,
                                                  sepBy, some, spaces,
                                                  stringLiteral, try,
                                                  whiteSpace)

newtype MdBlock
  = MkBody { body :: String }
  deriving (Eq, Show)

data MdHeading
  = MkHeading Int String
  deriving (Eq, Show)

data Section
  = MkSection
      { _title :: String
      , _body  :: String
      }
  deriving (Eq, Show)

type Chapters = [Section]

parseSection :: Parser Section
parseSection = do
  (MkHeading _ title) <- parseHeading
  (MkBody content) <- parseContent
  pure $ MkSection title content

parseMd :: Parser Chapters
parseMd = some parseSection

parseContent :: Parser MdBlock
parseContent = do
  MkBody <$> manyTill anyChar (eof <|> try (lookAhead nextHeader) <* newline)
  where nextHeader = do
        newline
        char '#'
        return ()

parseHeading :: Parser MdHeading
parseHeading = do
  hashes <- some $ char '#'
  spaces
  title <- manyTill anyChar newline
  pure $ MkHeading (length hashes) title


parseReadme :: IO ()
parseReadme = do
  result <- parseFromFile parseMd "README.md"
  case result of
    Nothing -> return ()
    Just s  -> print $ length <$> result
