{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Main where

import Control.Foldl (list)
import Data.List (foldl1', intersperse)
import Turtle

data Libraries = Libraries { resolver  :: Text
                           , libraries :: [Text]
                           } deriving (Eq, Show)

main :: IO ()
main = do
  let lib = Libraries "lts-6.1" ["turtle", "foldl"]
  libLocations <- flip fold list
                $ grep (contains . text $ resolver lib)
                $ grep (foldl1 (<|>) $ map (contains . text) (libraries lib))
                $ grep (ends . text $ ".conf")
                $ format fp <$> lstree "/home/carlo/.stack/snapshots"
  let libString = foldl1' (<>) . intersperse " " $ libLocations
  command "stack exec -- haddocset -t nuovo --no-global-packages create"
  command "stack build --haddock"
  command ("stack exec -- haddocset -t nuovo.docset add " <> libString)
  command "tar --exclude='.DS_Store' -cvzf nuovo.tgz nuovo.docset"

command :: MonadIO m => Text -> m ()
command c = void (shell c empty)
