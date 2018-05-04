{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types
    ( Version (..)
    , RequestDocument (..)
    , ResponseDocument (..)
    , ResponseLocations (..)
    , ResponseDiseases (..)
    ) where

import GHC.Generics
import Data.Aeson
import Data.Aeson.TH
import Servant

data Version = Version 
  { version  :: String
  , message  :: String   
  }
  deriving Show

data RequestDocument = RequestDocument { document :: String } deriving (Show, Generic)
data ResponseDocument = ResponseDocument { document :: String } deriving (Show, Generic)

data ResponseLocations = ResponseLocations { locations :: [String] } deriving (Show, Generic)
data ResponseDiseases = ResponseDiseases { locations :: [String] } deriving (Show, Generic)