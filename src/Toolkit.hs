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
    , multipleRegexRules
    ) where
        
import Data.List.Utils (replace)
import qualified Data.Map.Strict as SM
import Text.Regex.PCRE
import Data.Maybe

replaceArabicChars :: String -> String
replaceArabicChars document = " " ++ replace "ك" "ک" (replace "ي" "ی" document) ++ " "

removeStopwords :: String -> SM.Map String Integer -> String
removeStopwords document stopwordsMap = unwords (filter (\x -> not $ SM.member x stopwordsMap) (words (replaceArabicChars document)))

removePunctuations :: String -> SM.Map Char Integer -> String
removePunctuations document punctuationsMap = filter (\x -> not $ SM.member x punctuationsMap) (replaceArabicChars document)

removeSpaces :: String -> String
removeSpaces = unwords.words

multipleRegexRules :: String -> [String] -> [String]
multipleRegexRules document stringList = fmap fromJust $ filter isJust results
    where
        results = fmap (\p -> singleRegexRule document p) stringList

singleRegexRule :: String -> String -> Maybe String
singleRegexRule "" _ = Nothing
singleRegexRule document string = case ((replaceArabicChars document) =~ (" " ++ string ++ " ") :: Bool) of
    False -> Nothing
    True -> Just string
