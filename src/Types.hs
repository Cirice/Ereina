{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types
    ( Version (..)
    , FixSpacesRequest (..)
    , FixSpacesResponse (..)
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




data FixSpacesRequest = FixSpacesRequest { document :: String } deriving (Show, Generic)
data FixSpacesResponse = FixSpacesResponse { document :: String } deriving (Show, Generic)

