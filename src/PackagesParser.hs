module PackagesParser where

import Text.Megaparsec
import Text.Megaparsec.Char(char, string, alphaNumChar)
import Text.Megaparsec.Debug(dbg)
import Text.Megaparsec.Error(ParseErrorBundle)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void
import Data.Maybe(fromMaybe)
import Data.List (find)
import Control.Monad.Combinators(sepBy, between)
import Control.Monad(void)
import Packages

type Parser = Parsec Void Text
data PropertyValue = Text Text | Deps [Dependency]

readPackagesFromFile :: String -> IO [Package]
readPackagesFromFile filename = do
    fileContent <- TIO.readFile filename
    case readPackages fileContent of
        Left errors -> fail (show errors)
        Right packages -> return packages

readPackages :: Text -> Either (ParseErrorBundle Text Void) [Package]
readPackages fileContent = runParser (many pPackage) "Packages files" fileContent

pPackage :: Parser Package
pPackage = do
    props :: [(Text, PropertyValue)] <- (many $ try pProperty) 
    void (char '\n')
    let package = foldl updatePackage (Package "" "" []) props
    case packageName package of
        "" -> fail "Nameless package"
        _ -> return package

updatePackage :: Package -> (Text, PropertyValue) -> Package
updatePackage package ("Depends", (Deps deps)) = package { packageDependencies = deps }
updatePackage package ("Description", (Text desc)) = package { packageDescription = desc }
updatePackage package ("Package", (Text desc)) = package { packageName = desc }
updatePackage package _ = package

pProperty :: Parser (Text, PropertyValue)
pProperty = do
    key <- pFieldName <* (string ":") <* (optional $ char ' ')
    value <- case key of
        "Depends" -> Deps <$> (pDepends <* (char '\n'))
        _         -> Text <$> pValue
    return (key, value)

pFieldName :: Parser Text
pFieldName = pTextOfChars $ (alphaNumChar <|> char '-')

pDepends :: Parser [Dependency]
pDepends = pDepAlternatives `sepBy` (string ", ")
    
pDepAlternatives :: Parser [Text]
pDepAlternatives = pDependency `sepBy` (string " | ")

pDependency :: Parser Text
pDependency = pDepName <* (try $ optional pDepVersion)

pDepVersion :: Parser Text
pDepVersion = between (string " (") (char ')') (pTextOfChars $ anySingleBut ')')

pDepName :: Parser Text
pDepName = pTextOfChars $ (alphaNumChar <|> oneOf ['-', '.','+'])

pValue :: Parser Text
pValue = T.concat <$> ((:) <$> pLine <*> (many pContinuation))

pContinuation :: Parser Text
pContinuation = (T.append "\n") <$> (char ' ' >> pLine)

pLine :: Parser Text
pLine = (pTextOfChars $ anySingleBut '\n') <* (char '\n')

pTextOfChars :: Parser Char -> Parser Text
pTextOfChars ch = T.pack <$> (many $ ch)