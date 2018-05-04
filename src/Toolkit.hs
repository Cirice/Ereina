{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Toolkit
    ( removeSpaces
    , removeStopwords
    , removePunctuations
    , extractLocations
    , extractDiseases
    ) where

import qualified Data.Map.Strict as SM
import Text.Regex.PCRE
import Data.Maybe

removeStopwords :: String -> SM.Map String Integer -> String
removeStopwords document stopwordsMap = unwords (filter (\x -> not $ SM.member x stopwordsMap) (words document))

removePunctuations :: String -> SM.Map Char Integer -> String
removePunctuations document punctuationsMap = filter (\x -> not $ SM.member x punctuationsMap) document

removeSpaces :: String -> String
removeSpaces = unwords.words

extractLocations :: String -> [String] -> [String]
extractLocations document locations = fmap fromJust $ filter isJust results
    where
        results = fmap (\p -> getLocation document p) locations

getLocation :: String -> String -> Maybe String
getLocation "" _ = Nothing
getLocation document location = case (document =~ (" " ++ location) :: Bool) of
    False -> Nothing
    True -> Just location

extractDiseases :: String -> [String] -> [String]
extractDiseases document diseases = fmap fromJust $ filter isJust results
    where
        results = fmap (\p -> getDiseases document p) diseases

getDiseases :: String -> String -> Maybe String
getDiseases "" _ = Nothing
getDiseases document disease = case (document =~ (" " ++ disease) :: Bool) of
    False -> Nothing
    True -> Just disease