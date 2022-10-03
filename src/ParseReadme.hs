{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module ParseReadme(parseReadme, Section(MkSection)) where
import           Control.Applicative.Combinators (choice, many, manyTill,
                                                  manyTill_, some, (<|>))
import           Control.Exception               (SomeException)
import           Data.Data                       (Data (toConstr), Typeable)
import           Data.Foldable                   (Foldable (foldr'))
import           Data.Function                   (on)
import           Data.List                       (groupBy)
import qualified Data.Map                        as M
import qualified Data.Sequence                   as S
import           Text.Parser.LookAhead           (LookAheadParsing (lookAhead))
import Text.Regex.TDFA
import           Text.Trifecta                   (CharParsing (anyChar, char, string),
                                                  Parser, Parsing (eof),
                                                  TokenParsing (someSpace),
                                                  charLiteral, newline,
                                                  parseFromFile, parseString,
                                                  sepBy, some, spaces,
                                                  stringLiteral, try,
                                                  whiteSpace)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Control.Monad.Cont (MonadTrans(lift))

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
  firstChar <- lookAhead anyChar
  case firstChar of
    '#' -> pure $ MkBody ""
    _   -> MkBody <$> manyTill anyChar (eof <|> try (lookAhead nextHeader) <* newline)
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


parseReadme :: MaybeT IO Chapters
parseReadme = MaybeT $ do
  chapters <- parseFromFile parseMd "README.md"
  pure $ filterToDays <$> chapters

filterToDays :: Chapters -> Chapters
filterToDays = filter (\(MkSection title body) -> isDayTitle title)
  where isDayTitle title = title =~ "Day [0-9]{1,2}" :: Bool
