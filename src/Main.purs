module Main where

import Prelude
import Data.Traversable (for)
import Effect (Effect)
import Effect.Console (log)
import Data.Array (last)
import Data.Maybe (Maybe(..))
import Day01 as D01
import Day02 as D02
import Day03 as D03
import Day04 as D04
import Day05 as D05
import Day06 as D06
import Day07 as D07
import Day08 as D08
import Day09 as D09

allDays :: Array (Effect Unit)
allDays = [D01.main, D02.main, D03.main, D04.main, D05.main, D06.main, D07.main, D08.main, D09.main]

runAll :: Effect Unit
runAll = do
  log "Running all..."
  _ <- for allDays \d -> d
  pure unit

runLatest :: Effect Unit
runLatest = do
  log "Running todays..."
  case last allDays of
      Just day -> day
      _ -> pure unit

main :: Effect Unit
main = runLatest
