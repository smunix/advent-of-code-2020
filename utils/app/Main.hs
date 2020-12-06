{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Import
import Language.Haskell.TH.Syntax (liftString)
import Options.Applicative.Simple
import qualified Paths_utils
import RIO.Process
import Run
import System.Environment (getEnv)

main :: IO ()
main = do
  (options, ()) <-
    simpleOptions
      $(simpleVersion Paths_utils.version)
      "Header for command line arguments"
      "Program description, also for command line arguments"
      ( Options
          <$> switch
            ( long "verbose"
                <> short 'v'
                <> help "Verbose output?"
            )
          <*> strOption
            ( long "info"
                <> short 'i'
                <> help "Info from flake?"
            )
      )
      empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo \lf ->
    let app =
          App
            { appLogFunc = lf,
              appProcessContext = pc,
              appOptions = options
            }
     in runRIO app run
  where
    version :: String
    version = $(simpleVersion Paths_utils.version)
