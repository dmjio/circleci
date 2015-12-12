{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
module Web.CircleCI.Types where

import           GHC.Generics
import           Data.Char
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import qualified Data.Text as T
import           Data.Text (Text)
import           Servant.Common.Text
import           Servant.Client
import           Data.String

-- | `BaseUrl` which all CircleCI API requests use
host :: BaseUrl
host = BaseUrl Https "circleci.com" 443

-- | API Key
newtype APIKey = APIKey Text
  deriving (ToText, Eq, Show, IsString)

-- | UserName
newtype UserName = UserName Text
  deriving (ToText, Eq, Show, IsString)

-- | Project
newtype Project = Project Text
  deriving (ToText, Eq, Show, IsString)

-- | Limit
newtype Limit = Limit Integer
  deriving (ToText, FromText, Eq, Show)

-- | Offset
newtype Offset = Offset Integer
  deriving (ToText, FromText, Eq, Show)

-- | Filter
data Filter = Completed | Successful | Failed | Running
  deriving (Eq, Show, Generic)

instance ToText Filter where toText = T.toLower . T.pack . show

-- | BuildNum
newtype BuildNum = BuildNum Integer
  deriving (Eq, Show, Generic, ToText)

-- | Branch
newtype Branch = Branch Text
  deriving (Eq, Show, Generic, ToText)


-- | Build Params
data BuildParams = BuildParams {
    bpParallel :: Maybe Integer
    -- ^ The number of containers to use to run the build. Default is null and the project default is used.
  , bpRevision :: Maybe T.Text
    -- ^ The specific revision to build. Default is null and the head of the branch is used
  , bpBuildParameters :: Object
  } deriving (Show, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2
                            } ''BuildParams)


-- | Environment variable to add
data Var = Var {
    varName :: Text
  , varValue :: Text
  } deriving (Show, Eq)

$(deriveJSON defaultOptions { fieldLabelModifier = map toLower . drop 3 } ''Var)

-- | EnvName
newtype EnvName = EnvName Text
  deriving (ToText, Eq, Show)

data KeyType = GithubKey | DeployKey
  deriving (Show, Eq)

instance ToJSON KeyType where
  toJSON DeployKey = object [ "type" .= ("deploy-key" :: Text) ]
  toJSON GithubKey = object [ "type" .= ("github-user-key" :: Text) ] 
