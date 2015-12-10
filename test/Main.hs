{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.URI as URI
import qualified Network.URI.TLD as URI
import System.Exit (exitFailure)
import Test.HUnit

main :: IO ()
main = do
    Counts _ _ errC failC <- runTestTT tests
    if errC + failC == 0 then
        return ()
    else
        exitFailure


    where
        tests = TestLabel "Top level domain parser tests" $ TestList [ 
              makeTests "http://localhost:3000/sdfsdf" "" "" "localhost"
            , makeTests "https://localhost:3000/sdfsdf" "" "" "localhost"
            , makeTests "https://google.com" "" "google" "com"
            , makeTests "http://www.theguardian.co.uk/world" "www" "theguardian" "co.uk"
            ]


makeTests :: Text -> Text -> Text -> Text -> Test
makeTests url subdomain domain tld = TestList [
      TestCase $ assertEqual "parseTLD failed" (URI.parseTLD $ Text.unpack url) result
    , TestCase $ assertEqual "parseTLDText failed" (URI.parseTLDText url) result
    , TestCase $ case URI.parseURI $ Text.unpack url of
        Nothing -> 
            assertFailure $ "Invalid test: " ++ Text.unpack url
        Just uri ->
            assertEqual "parseTLDURL failed" (URI.parseTLDURI uri) result
    ]

    where
        result = Just (subdomain, domain, tld)
