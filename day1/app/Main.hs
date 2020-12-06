{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Day1.Import
import Day1.Run
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_day1

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_day1.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
    )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
     in runRIO app run
