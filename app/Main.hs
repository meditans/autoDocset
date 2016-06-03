{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Main where

import Control.Foldl   (list)
import Data.List       (foldl1', intersperse)
import Options.Generic (ParseRecord, Generic, getRecord)
import Turtle

data Options = Options { docsetName :: Text
                       , resolver   :: Text
                       , libraries  :: [Text]
                       } deriving (Eq, Generic, Show)

instance ParseRecord Options

-- Make sure haddocset is installed. Example usage:
-- stack exec autoDocset -- --docsetName nuovissimo --resolver lts-6.1 --libraries turtle --libraries foldl

main :: IO ()
main = do
  opt <- getRecord "autoDocset"
  libLocations <- flip fold list
    $ grep (contains . text $ resolver opt)
    $ grep (foldl1 (<|>) $ map (contains . text) (libraries opt))
    $ grep (ends . text $ ".conf")
    $ format fp <$> lstree "/home/carlo/.stack/snapshots"
  let libString = foldl1' (<>) . intersperse " " $ libLocations
  command ("stack exec -- haddocset -t " <> docsetName opt <> " --no-global-packages create")
  command "stack build --haddock"
  command ("stack exec -- haddocset -t " <> docsetName opt <> ".docset add " <> libString)
  command ("tar --exclude='.DS_Store' -cvzf " <> docsetName opt <> ".tgz " <> docsetName opt <> ".docset")
  command ("rm -r " <> docsetName opt <> ".docset")

command :: MonadIO m => Text -> m ()
command c = void (shell c empty)
