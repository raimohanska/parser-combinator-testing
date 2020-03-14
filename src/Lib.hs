module Lib where

import Text.Megaparsec
import Text.Megaparsec.Char(char, string, alphaNumChar)
import Text.Megaparsec.Debug(dbg)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void
import Data.Maybe(fromMaybe)
import Data.List (find)
import Control.Monad.Combinators(sepBy, between)

type Parser = Parsec Void Text

readPackages :: IO [Package]
readPackages = do
    fileContent <- TIO.readFile "status.real.txt"
    case runParser (many pPackage) "Packages files" fileContent of
        Left errors -> fail "Unable to parse deps"
        Right packages -> return packages

data Package = Package
  { packageName :: Text,
    packageDescription :: Text,
    packageDependencies :: [Dependency]
  } deriving (Eq, Show)

type Dependency = [Text]

pPackage :: Parser Package
pPackage = do
    props <- (many $ try $ pProperty) <* (many $ char '\n')
    Package <$> getProp "Package" props <*> getProp "Description" props <*> parseDeps props

parseDeps props = case findProp "Depends" props of
    Just depString -> case runParser pDepends "Depends field" depString of
        Left errors -> fail "Unable to parse deps" -- TODO error passing
        Right text -> return text
    Nothing -> return []

findProp :: Text -> [(Text, Text)] -> Maybe Text
findProp name props = fmap snd $ (find (\case (a, b) -> a == name) props)

getProp :: Text -> [(Text, Text)] -> Parser Text
getProp name props = do
    let maybeValue = findProp name props
    case maybeValue of
        Just v -> return v
        Nothing -> fail $ show $ T.append "Missing property " name

pProperty :: Parser (Text, Text)
pProperty = do
    key <- pFieldName <* (string ":") <* (optional $ char ' ')
    value <- pValue    
    return (key, value)

pDepends :: Parser [Dependency]
pDepends = pDepAlternatives `sepBy` (string ", ")
    
pDepAlternatives :: Parser [Text]
pDepAlternatives = pDep `sepBy` (string " | ")

pDep :: Parser Text
pDep = pDepName <* (try $ optional pDepVersion)

pDepVersion :: Parser Text
pDepVersion = between (string " (") (char ')') (pTextOfChars $ anySingleBut ')')

pDepName :: Parser Text
pDepName = pTextOfChars $ (alphaNumChar <|> oneOf ['-', '.','+'])

pFieldName :: Parser Text
pFieldName = pTextOfChars $ (alphaNumChar <|> char '-')

pValue :: Parser Text
pValue = T.concat <$> ((:) <$> pLine <*> (many pContinuation))

pContinuation :: Parser Text
pContinuation = (T.append "\n") <$> (char ' ' >> pLine)

pLine :: Parser Text
pLine = (pTextOfChars $ anySingleBut '\n') <* (char '\n')

pTextOfChars :: Parser Char -> Parser Text
pTextOfChars ch = T.pack <$> (many $ ch)