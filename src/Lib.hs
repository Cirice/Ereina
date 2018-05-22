{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.List
import Control.Monad.IO.Class
import Prelude
import Data.Char
import qualified Data.Map.Strict as SM
import Types
import Data.List.Utils 
import Toolkit
import qualified Internal.Rules as Rules


$(deriveJSON defaultOptions ''RequestDocument)
$(deriveJSON defaultOptions ''Version)

instance ToJSON ResponseDocument
instance ToJSON ResponseLocations
instance ToJSON ResponseDiseases
instance ToJSON ResponseBrands

type API = "version" :> Get '[JSON] Version  
      :<|> "fixSpaces" :> ReqBody '[JSON] RequestDocument :> Post '[JSON] ResponseDocument
      :<|> "dropStopwords" :> ReqBody '[JSON] RequestDocument :> Post '[JSON] ResponseDocument
      :<|> "dropPunctuations" :> ReqBody '[JSON] RequestDocument :> Post '[JSON] ResponseDocument
      :<|> "extractLocations" :> ReqBody '[JSON] RequestDocument :> Post '[JSON] ResponseLocations
      :<|> "extractDiseases" :> ReqBody '[JSON] RequestDocument :> Post '[JSON] ResponseDiseases
      :<|> "extractBrands" :> ReqBody '[JSON] RequestDocument :> Post '[JSON] ResponseBrands

-- | Returns API version as a JSON object
version' :: Version
version' = Version "0.1.1.1" "Hi there, I am Ereina."

startApp :: IO ()
startApp = run 2319 app

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

server :: Server API
server = return version'
    :<|> fixSpaces  
    :<|> dropStopwords
    :<|> dropPunctuations
    :<|> getLocations
    :<|> getDiseases
    :<|> getBrands
  where 
        fixSpaces :: RequestDocument -> Handler ResponseDocument
        fixSpaces (RequestDocument document) = return (ResponseDocument { document = (removeSpaces document)})
        
        dropStopwords :: RequestDocument -> Handler ResponseDocument
        dropStopwords (RequestDocument document) = return (ResponseDocument { document = (removeStopwords document Rules.stopwordsMap)})

        dropPunctuations :: RequestDocument -> Handler ResponseDocument
        dropPunctuations (RequestDocument document) = return (ResponseDocument { document = (removePunctuations document Rules.punctuationsMap)})     

        getLocations :: RequestDocument -> Handler ResponseLocations
        getLocations (RequestDocument document) = return (ResponseLocations { locations = (multipleRegexRules document Rules.locations)})  

        getDiseases :: RequestDocument -> Handler ResponseDiseases
        getDiseases (RequestDocument document) = return (ResponseDiseases { diseases = (multipleRegexRules document Rules.diseases)})      
        
        getBrands :: RequestDocument -> Handler ResponseBrands
        getBrands (RequestDocument document) = return (ResponseBrands { brands = (multipleRegexRules document Rules.brands)})      