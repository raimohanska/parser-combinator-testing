module Packages where

import Data.Text (Text)

data Package = Package
  { packageName :: Text,
    packageDescription :: Text,
    packageDependencies :: [Dependency]
  } deriving (Eq, Show)

type Dependency = [Text]