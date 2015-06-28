{-# LANGUAGE TemplateHaskell #-}
module Network.URI.TLD.Internal where

import Control.Monad
import qualified Data.Char as Char
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Language.Haskell.TH.Syntax

tldSet :: Q Exp
tldSet = do
    tlds' <- parseTLDFile
    let tlds = ("localhost"):tlds'
    let tldsE = ListE $ map (LitE . StringL) tlds
    return $ AppE (VarE 'Set.fromList) tldsE

    where
        parseTLDFile = runIO $ do
            let fp = "data/tld.dat"
            contents <- Text.readFile fp
            let lines = Text.lines contents
            foldM helper [] lines

        helper acc line' = 
            let line = Text.strip line' in
            let lineS = Text.unpack line in
            -- Skip empty lines and lines with non-alphanumeric characters. 
            if Text.null line || Text.any (\c -> not (Char.isAlphaNum c) && c /= '.' && c /= '-') line then do
                -- putStrLn $ "Skipping: " ++ lineS
                return acc
            else
                return $ lineS:acc

