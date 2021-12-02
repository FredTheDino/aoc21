module Main where

import Prelude
import Data.Traversable (for)
import Effect (Effect)
import Effect.Console (log)
import Data.Array (last)
import Data.Maybe (Maybe(..))
import Day01 as D01
import Day02 as D02

allDays = [D01.main, D02.main]

runAll :: Effect _
runAll = do
  log "Running all..."
  for allDays \d -> d

runLatest :: Effect _
runLatest = do
  log "Running todays..."
  case last allDays of
      Just day -> day
      _ -> pure unit

main :: _
main = runLatest
