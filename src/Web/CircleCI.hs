{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds     #-}
module Web.CircleCI where

import Control.Monad.Trans.Either
import Data.Aeson
import Data.String
import Data.Proxy
import qualified Data.Text as T
import Data.Text ( Text )
import Servant.API
import Servant.Client
import Servant.Common.Text
import Web.CircleCI.Types

type CircleToken = QueryParam "circle-token" APIKey

-- | GET /me
type Me =
     "api"
  :> "v1"
  :> CircleToken
  :> "me"
  :> Get '[JSON] Value

-- | Provides information about the signed in user.
me :: APIKey -> IO (Either ServantError Value)
me key = runEitherT $ req (Just key)
  where
    req :: Maybe APIKey -> EitherT ServantError IO Value
    req = client (Proxy :: Proxy Me) host

-- | Get /projects
type GetProjects =
     "api"
  :> "v1"
  :> CircleToken
  :> "projects"
  :> Get '[JSON] Value

-- | List of all the projects you're following on CircleCI
-- with build information organized by branch
getProjects :: APIKey -> IO (Either ServantError Value)
getProjects key = runEitherT $ req (Just key)
  where
    req :: Maybe APIKey -> EitherT ServantError IO Value
    req = client (Proxy :: Proxy GetProjects) host

-- | Get Projects API
type BuildSummary =
     "api"
  :> "v1"
  :> CircleToken
  :> Capture "username" UserName
  :> Capture "project" Project
  :> QueryParam "limit" Limit
  :> QueryParam "offset" Offset
  :> QueryParam "filter" Filter
  :> Get '[JSON] Value

-- | Build summary for each of the last 30 builds for a single git repo.
getBuildSummary
  :: APIKey
  -> UserName
  -> Project
  -> Maybe Limit
  -> Maybe Offset
  -> Maybe Filter
  -> IO (Either ServantError Value)
getBuildSummary key uname pid lim offst filter' =
    runEitherT $ req (Just key) uname pid lim offst filter'
  where
    req :: Maybe APIKey
        -> UserName
        -> Project
        -> Maybe Limit
        -> Maybe Offset
        -> Maybe Filter
        -> EitherT ServantError IO Value
    req = client (Proxy :: Proxy BuildSummary) host

-- | Get Projects API
type RecentBuilds =
     "api"
  :> "v1"
  :> CircleToken
  :> QueryParam "limit" Limit
  :> QueryParam "offset" Offset
  :> "recent-builds"
  :> Get '[JSON] Value

-- | Build summary for each of the last 30 recent builds, ordered by build_num.
getRecentBuilds
  :: APIKey
  -> Maybe Limit
  -> Maybe Offset
  -> IO (Either ServantError Value)
getRecentBuilds key lim offst =
    runEitherT $ req (Just key) lim offst
  where
    req :: Maybe APIKey
        -> Maybe Limit
        -> Maybe Offset
        -> EitherT ServantError IO Value
    req = client (Proxy :: Proxy RecentBuilds) host

-- | GET: /project/:username/:project/:build_num
type SingleBuild =
     "api"
  :> "v1"
  :> CircleToken
  :> "project"
  :> Capture "username" UserName
  :> Capture "project" Project
  :> Capture "build_num" BuildNum
  :> Get '[JSON] Value

-- | Full details for a single build. The response includes all of the fields from the build summary.
-- This is also the payload for the notification webhooks,
-- in which case this object is the value to a key named 'payload'.
getBuild
  :: APIKey
  -> UserName
  -> Project
  -> BuildNum
  -> IO (Either ServantError Value)
getBuild key name pid num =
    runEitherT $ req (Just key) name pid num 
  where
    req :: Maybe APIKey
        -> UserName
        -> Project
        -> BuildNum
        -> EitherT ServantError IO Value
    req = client (Proxy :: Proxy SingleBuild) host

-- | GET: /project/:username/:project/:build_num/artifacts
type BuildArtifacts =
     "api"
  :> "v1"
  :> CircleToken
  :> "project"
  :> Capture "username" UserName
  :> Capture "project" Project
  :> Capture "build_num" BuildNum
  :> "artifacts"
  :> Get '[JSON] Value

-- | List the artifacts produced by a given build.
getBuildArtifacts
  :: APIKey
  -> UserName
  -> Project
  -> BuildNum
  -> IO (Either ServantError Value)
getBuildArtifacts key name pid num =
    runEitherT $ req (Just key) name pid num 
  where
    req :: Maybe APIKey
        -> UserName
        -> Project
        -> BuildNum
        -> EitherT ServantError IO Value
    req = client (Proxy :: Proxy BuildArtifacts) host


-- | POST: /project/:username/:project/:build_num/retry
type RetryBuild =
     "api"
  :> "v1"
  :> CircleToken
  :> "project"
  :> Capture "username" UserName
  :> Capture "project" Project
  :> Capture "build_num" BuildNum
  :> "retry"
  :> Post '[JSON] Value

-- | Retries the build, returns a summary of the new build
retryBuild
  :: APIKey
  -> UserName
  -> Project
  -> BuildNum
  -> IO (Either ServantError Value)
retryBuild key name pid num =
    runEitherT $ req (Just key) name pid num 
  where
    req :: Maybe APIKey
        -> UserName
        -> Project
        -> BuildNum
        -> EitherT ServantError IO Value
    req = client (Proxy :: Proxy RetryBuild) host

-- | POST: /project/:username/:project/:build_num/ssh
type RetryBuildSSH =
     "api"
  :> "v1"
  :> CircleToken
  :> "project"
  :> Capture "username" UserName
  :> Capture "project" Project
  :> Capture "build_num" BuildNum
  :> "ssh"
  :> Post '[JSON] Value

-- | Retries the build w/ ssh, returns a summary of the new build
retryBuildSSH
  :: APIKey
  -> UserName
  -> Project
  -> BuildNum
  -> IO (Either ServantError Value)
retryBuildSSH key name pid num =
    runEitherT $ req (Just key) name pid num 
  where
    req :: Maybe APIKey
        -> UserName
        -> Project
        -> BuildNum
        -> EitherT ServantError IO Value
    req = client (Proxy :: Proxy RetryBuildSSH) host

-- | POST: /api/v1/project/:username/:project/:build_num/ssh-users?circle-token=:token
type AddUserToBuild =
     "api"
  :> "v1"
  :> CircleToken
  :> "project"
  :> Capture "username" UserName
  :> Capture "project" Project
  :> Capture "build_num" BuildNum
  :> "ssh-users"
  :> Post '[JSON] Value

-- | Only available when using a user API token.
-- If the current user has permission to build the project,
-- this API adds the current user's SSH public key to the authorized
-- keys on each container running a build.
-- This allows them to SSH to the build containers.
addUserToBuild
  :: APIKey
  -> UserName
  -> Project
  -> BuildNum
  -> IO (Either ServantError Value)
addUserToBuild key name pid num =
    runEitherT $ req (Just key) name pid num 
  where
    req :: Maybe APIKey
        -> UserName
        -> Project
        -> BuildNum
        -> EitherT ServantError IO Value
    req = client (Proxy :: Proxy AddUserToBuild) host

-- | POST https://circleci.com/api/v1/project/:username/:project/:build_num/cancel?circle-token=:token
type CancelBuild =
     "api"
  :> "v1"
  :> CircleToken
  :> "project"
  :> Capture "username" UserName
  :> Capture "project" Project
  :> Capture "build_num" BuildNum
  :> "cancel"
  :> Post '[JSON] Value

-- | Cancels the build, returns a summary of the build.
cancelBuild
  :: APIKey
  -> UserName
  -> Project
  -> BuildNum
  -> IO (Either ServantError Value)
cancelBuild key name pid num =
    runEitherT $ req (Just key) name pid num 
  where
    req :: Maybe APIKey
        -> UserName
        -> Project
        -> BuildNum
        -> EitherT ServantError IO Value
    req = client (Proxy :: Proxy CancelBuild) host

-- | POST https://circleci.com/api/v1/project/:username/:project/:build_num/cancel?circle-token=:token
type TriggerBuild =
     "api"
  :> "v1"
  :> CircleToken
  :> "project"
  :> ReqBody '[JSON] BuildParams
  :> Capture "username" UserName
  :> Capture "project" Project
  :> "tree"
  :> Capture "branch" Branch
  :> Post '[JSON] Value

-- https://circleci.com/api/v1/project/:username/:project/tree/:branch?circle-token=:token

triggerDevDeploy :: IO (Either ServantError Value)
triggerDevDeploy =
  triggerBuild (APIKey "2a5fcc932eb9c0f584e6bb5253f159596b6357c9")
               (BuildParams Nothing Nothing [("ENV", String "dev")])
               (UserName "vertigomedia")
               (Project "Vertigo-Social")
               (Branch "master")

-- | Cancels the build, returns a summary of the build.
triggerBuild
  :: APIKey
  -> BuildParams
  -> UserName
  -> Project
  -> Branch
  -> IO (Either ServantError Value)
triggerBuild key name pid params branch =
    runEitherT $ req (Just key) name pid params branch
  where
    req :: Maybe APIKey
        -> BuildParams
        -> UserName
        -> Project
        -> Branch
        -> EitherT ServantError IO Value
    req = client (Proxy :: Proxy TriggerBuild) host

-- | DELETE https://circleci.com/api/v1/project/:username/:project/build-cache?circle-token=:token
type ClearCache =
     "api"
  :> "v1"
  :> CircleToken
  :> "project"
  :> Capture "username" UserName
  :> Capture "project" Project
  :> "build-cache"
  :> Delete '[JSON] Value

-- | Cancels the build, returns a summary of the build.
clearCache
  :: APIKey
  -> UserName
  -> Project
  -> IO (Either ServantError Value)
clearCache key name pid =
    runEitherT $ req (Just key) name pid 
  where
    req :: Maybe APIKey
        -> UserName
        -> Project
        -> EitherT ServantError IO Value
    req = client (Proxy :: Proxy ClearCache) host

-- | GET /api/v1/project/:username/:project/envvar?circle-token=:token
type ListEnvironment =
     "api"
  :> "v1"
  :> "project"
  :> CircleToken
  :> Capture "username" UserName
  :> Capture "project" Project
  :> "envvar"
  :> Get '[JSON] Value

-- | Cancels the build, returns a summary of the build.
listEnvironment
  :: APIKey
  -> UserName
  -> Project
  -> IO (Either ServantError Value)
listEnvironment key name pid =
    runEitherT $ req (Just key) name pid 
  where
    req :: Maybe APIKey
        -> UserName
        -> Project
        -> EitherT ServantError IO Value
    req = client (Proxy :: Proxy ListEnvironment) host

-- | POST
type AddEnvironment =
     "api"
  :> "v1"
  :> "project"
  :> CircleToken
  :> Capture "username" UserName
  :> Capture "project" Project
  :> ReqBody '[JSON] Var
  :> "envvar"
  :> Post '[JSON] Value

-- | Cancels the build, returns a summary of the build.
addEnvironment
  :: APIKey
  -> UserName
  -> Project
  -> Var
  -> IO (Either ServantError Value)
addEnvironment key name env project =
    runEitherT $ req (Just key) name env project
  where
    req :: Maybe APIKey
        -> UserName
        -> Project
        -> Var
        -> EitherT ServantError IO Value
    req = client (Proxy :: Proxy AddEnvironment) host

-- | DELETE
type DeleteEnvironment =
     "api"
  :> "v1"
  :> "project"
  :> CircleToken
  :> Capture "username" UserName
  :> Capture "project" Project
  :> "envvar"
  :> Capture "name" EnvName
  :> Delete '[JSON] Value

-- | Deletes the environment variable named ':name'
deleteEnvironment
  :: APIKey
  -> UserName
  -> Project
  -> EnvName
  -> IO (Either ServantError Value)
deleteEnvironment key name project env =
    runEitherT $ req (Just key) name project env 
  where
    req :: Maybe APIKey
        -> UserName
        -> Project
        -> EnvName
        -> EitherT ServantError IO Value
    req = client (Proxy :: Proxy DeleteEnvironment) host

-- | GET /api/v1/project/:username/:project/checkout-key?circle-token=:token
type CheckoutKeys =
     "api"
  :> "v1"
  :> "project"
  :> CircleToken
  :> Capture "username" UserName
  :> Capture "project" Project
  :> "checkout-key"
  :> Get '[JSON] Value

-- | Lists the checkout keys for :project
listCheckoutKeys
  :: APIKey
  -> UserName
  -> Project
  -> IO (Either ServantError Value)
listCheckoutKeys key name project =
    runEitherT $ req (Just key) name project
  where
    req :: Maybe APIKey
        -> UserName
        -> Project
        -> EitherT ServantError IO Value
    req = client (Proxy :: Proxy CheckoutKeys) host

type NewCheckoutKey =
     "api"
  :> "v1"
  :> "project"
  :> CircleToken
  :> Capture "username" UserName
  :> Capture "project" Project
  :> ReqBody '[JSON] KeyType
  :> Post '[JSON] Value

-- | Creates a new checkout key. Only usable with a user API token.
newCheckoutKey
  :: APIKey
  -> UserName
  -> Project
  -> KeyType
  -> IO (Either ServantError Value)
newCheckoutKey key name project keyType =
    runEitherT $ req (Just key) name project keyType
  where
    req :: Maybe APIKey
        -> UserName
        -> Project
        -> KeyType
        -> EitherT ServantError IO Value
    req = client (Proxy :: Proxy NewCheckoutKey) host

type GetCheckoutKey =
     "api"
  :> "v1"
  :> "project"
  :> CircleToken
  :> Capture "username" UserName
  :> Capture "project" Project
  :> "checkout-key"
  :> Capture "fingerprint" FingerPrint
  :> Get '[JSON] Value

-- | Creates a new checkout key. Only usable with a user API token.
getCheckoutKey
  :: APIKey
  -> UserName
  -> Project
  -> FingerPrint
  -> IO (Either ServantError Value)
getCheckoutKey key name project fingerPrint =
    runEitherT $ req (Just key) name project fingerPrint
  where
    req :: Maybe APIKey
        -> UserName
        -> Project
        -> FingerPrint
        -> EitherT ServantError IO Value
    req = client (Proxy :: Proxy GetCheckoutKey) host

type DeleteCheckoutKey =
     "api"
  :> "v1"
  :> "project"
  :> CircleToken
  :> Capture "username" UserName
  :> Capture "project" Project
  :> "checkout-key"
  :> Capture "fingerprint" FingerPrint
  :> Delete '[JSON] Value

-- | Creates a new checkout key. Only usable with a user API token.
deleteCheckoutKey
  :: APIKey
  -> UserName
  -> Project
  -> FingerPrint
  -> IO (Either ServantError Value)
deleteCheckoutKey key name project fingerPrint =
    runEitherT $ req (Just key) name project fingerPrint
  where
    req :: Maybe APIKey
        -> UserName
        -> Project
        -> FingerPrint
        -> EitherT ServantError IO Value
    req = client (Proxy :: Proxy DeleteCheckoutKey) host

-- | GET /api/v1/project/:username/:project/:build_num/tests?circle-token=:token
type TestMetaData =
     "api"
  :> "v1"
  :> "project"
  :> CircleToken
  :> Capture "username" UserName
  :> Capture "project" Project
  :> Capture "build_num" BuildNum
  :> Delete '[JSON] Value

-- | Provides test metadata for a build
testMetaData
  :: APIKey
  -> UserName
  -> Project
  -> BuildNum
  -> IO (Either ServantError Value)
testMetaData key name project fingerPrint =
    runEitherT $ req (Just key) name project fingerPrint
  where
    req :: Maybe APIKey
        -> UserName
        -> Project
        -> BuildNum
        -> EitherT ServantError IO Value
    req = client (Proxy :: Proxy TestMetaData) host

-- | GET /api/v1/project/:username/:project/:build_num/tests?circle-token=:token
type SSHKeys =
     "api"
  :> "v1"
  :> "project"
  :> CircleToken
  :> Capture "username" UserName
  :> Capture "project" Project
  :> ReqBody '[JSON] PrivateKey
  :> Post '[JSON] Value

-- | Creates an ssh key that will be used to access the external system identified by the hostname parameter for SSH key-based authentication.
createSSHKey
  :: APIKey
  -> UserName
  -> Project
  -> PrivateKey
  -> IO (Either ServantError Value)
createSSHKey key name project privateKey =
    runEitherT $ req (Just key) name project privateKey
  where
    req :: Maybe APIKey
        -> UserName
        -> Project
        -> PrivateKey
        -> EitherT ServantError IO Value
    req = client (Proxy :: Proxy SSHKeys) host
