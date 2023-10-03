module Main where

import Spec

import Test.Syd

main :: IO ()
main = do
  -- sydTest spec

  sydTestWith defaultSettings
    { settingRetries = 0
    -- , settingFilters = ["FOCUS"]
    , settingGoldenReset = True
    , settingGoldenStart = True
    } spec
