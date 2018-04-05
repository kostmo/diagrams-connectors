#!/usr/bin/env runhaskell

import           Data.Monoid            ((<>))
import           Options.Applicative

import qualified Command.TestConnectors as TestConnectors

main :: IO ()
main = execParser opts >>= TestConnectors.mainAppCode
  where
    opts = info (helper <*> TestConnectors.myCliParser)
      ( fullDesc
     <> progDesc "Generate connectors demo SVG"
     <> header "test-connectors - generates connectors demo" )
