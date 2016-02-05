{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Main where

import           Turtle
import Control.Foldl (list)
import Data.List (intersperse, foldl1')

data Libraries = Libraries { resolver  :: Text
                           , libraries :: [Text]
                           } deriving (Eq, Show)

main :: IO ()
main = do
  let lib = Libraries "lts-5.1" ["turtle", "foldl"]
  libLocations <- flip fold list
        $ grep (contains . text $ resolver lib)
        $ grep (foldl1 (<|>) $ map (contains . text) (libraries lib))
        $ grep (ends . text $ ".conf")
        $ fmap (format fp) $ lstree "/home/carlo/.stack/snapshots"
  let libString = foldl1' (<>) . intersperse " " $ libLocations
  print libString
  void $ shell ("stack exec -- haddocset -t nuovo --no-global-packages create") empty
  void $ shell ("stack build --haddock") empty
  void $ shell ("stack exec -- haddocset -t target.docset add " <> libString) empty
  void $ shell ("tar --exclude='.DS_Store' -cvzf nuovo.tgz nuovo.docset") empty
