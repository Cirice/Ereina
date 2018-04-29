{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-|
Module      : Lib
Description : The module where the server resides
Copyright   : (c) Hossein Abedi Firouzjaee, 2018
License     : GPL-3
Maintainer  : abedi.hossein@protonmail.ch
Stability   : experimental
Portability : POSIX
-}

module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import GHC.Generics
import Servant
import qualified Data.Map.Strict as SM
import Data.List
import Prelude
import Data.Char
import Types
import Data.List.Utils 
import Toolkit

$(deriveJSON defaultOptions ''FixSpacesRequest)
$(deriveJSON defaultOptions ''Version)
instance ToJSON FixSpacesResponse

type API = "version" :> Get '[JSON] Version  
      :<|> "fixSpaces" :> ReqBody '[JSON] FixSpacesRequest :> Post '[JSON] FixSpacesResponse

-- | Returns API version as a JSON object
version' :: Version
version' = Version "0.1.0.0" "Hi there, I am Ereina."

startApp :: IO ()
startApp = run 2319 app

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

server :: Server API
server = return version'
    :<|> fixSpaces  
  where fixSpaces :: FixSpacesRequest -> Handler FixSpacesResponse
        fixSpaces (FixSpacesRequest document) = return (FixSpacesResponse {document = (removeSpaces document)})


