-- | Experimental HTML rendering of DBC content.
-- This is one of multiple "frontends", besides the DBC renderer and .compact.json renderer.
module Command.TestConnectors where

import           Data.Foldable          (for_)
import           Data.Monoid            ((<>))
import qualified Diagrams.Backend.SVG   as Svg
import qualified Diagrams.Prelude       as DP
import           Options.Applicative
import           System.Directory       (createDirectoryIfMissing)
import           System.FilePath.Posix

import qualified Command.ConnectorsDemo as ConnectorsDemo


diagramDimensions = DP.mkSizeSpec2D (Just 1200) (Just 1200)


testsAndOutputs = [
    (ConnectorsDemo.renderRawComplex, "complex-diagrams.svg")
  , (ConnectorsDemo.renderRawSimple, "simple-diagrams.svg")
  , (ConnectorsDemo.renderRawOverlaps, "overlapping-diagrams.svg")
  , (ConnectorsDemo.renderStraightBusScenarios, "straight-bus-diagrams.svg")
  ]


data MyCommandLineArgs = MyCommandLineArgs {
    outputDir :: FilePath
  , color     :: Bool
  , quiet     :: Bool
  }


myCliParser :: Parser MyCommandLineArgs
myCliParser = MyCommandLineArgs
  <$> output_dir_option
  <*> color_option
  <*> quiet_option
  where
    output_dir_option = strOption
       ( long "output-dir"
      <> metavar "OUTPUT_PATH"
      <> value "."
      <> help "Directory in which to generate the output files" )
    color_option = switch
       ( long "color"
      <> help "Does nothing; exists for compatibility with Test.Framework.defaultMain" )
    quiet_option = switch
       ( long "quiet"
      <> help "Suppress console output" )


mainAppCode :: MyCommandLineArgs -> IO ()
mainAppCode my_cli_args = do

  createDirectoryIfMissing True base_output_directory

  for_ testsAndOutputs $ \(d, p) ->
    Svg.renderPretty (base_output_directory </> p) diagramDimensions d

  where
    base_output_directory = outputDir my_cli_args
