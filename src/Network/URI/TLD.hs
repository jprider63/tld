{-# LANGUAGE OverloadedStrings #-}
module Network.URI.TLD (parseTLD, separateTLD, separateTLD') where

import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.URI as URI

tldSet = Set.fromList ["localhost", "com"]

-- | Separate the subdomain, domain, and TLD of a URI. 
separateTLD :: URI.URI -> Maybe (Text, Text, Text)
separateTLD uri = 
    case URI.uriAuthority uri of
        Nothing ->
            Nothing
        Just uriAuth ->
            separateTLD' $ Text.pack $ URI.uriRegName uriAuth

-- | Separate the subdomain, domain, and TLD of a @Text@ string.
separateTLD' :: Text -> Maybe (Text, Text, Text)
separateTLD' domain = helper "" "" $ Text.toLower domain
    where
        helper _ _ "" = Nothing
        helper subdomain domain tld = 
            if Set.member tld tldSet then
                Just (subdomain, domain, tld)
            else
                let subdomain' = 
                      if Text.null subdomain then
                        domain
                      else
                        Text.concat [subdomain, ".", domain] 
                in
                let (domain', tld') = Text.break (== '.') tld in
                let (head, tld'') = Text.splitAt 1 tld' in
                if head /= "." then
                    Nothing
                else
                    helper subdomain' domain' tld''
                        
-- | Parse a URI, and separate the subdomain, domain, and TLD.
parseTLD :: String -> Maybe (Text, Text, Text)
parseTLD s = case URI.parseURI s of 
    Nothing ->
        Nothing
    Just uri ->
        separateTLD uri

