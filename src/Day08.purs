module Day08 where

import Prelude
import Data.Array (fromFoldable, (!!), (:))
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (fromCharArray)
import Data.Ord (abs)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Async as FS
import Partial.Unsafe (unsafeCrashWith)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Combinators as PC
import Text.Parsing.Parser.String as PS
import Text.Parsing.Parser.Token as PT
import Debug

repeatParser :: forall p. Int -> P.Parser String p -> P.Parser String (Array p)
repeatParser n parser =
  if n > 0 then do
    r <- parser
    rest <- repeatParser (n - 1) parser
    pure $ r : rest
  else do
    pure []

wordParser :: P.Parser String (Array Char)
wordParser = do
  PS.skipSpaces
  letters <- PC.many1 PT.letter
  pure $ fromFoldable letters

type Line =
  { every :: Array (Array Char)
  , found :: Array (Array Char)
  }

lineParser :: P.Parser String Line
lineParser = do
  every <- repeatParser 10 wordParser
  _ <- PS.string " | "
  found <- repeatParser 4 wordParser
  pure $ { every, found }

type Input = Array Line

inputParser :: P.Parser String Input
inputParser = do
  cases <- PC.many1 (PC.try lineParser)
  pure
    $ cases
        # fromFoldable

parse :: forall t. Either t String -> Input
parse = case _ of
  Left _ -> unsafeCrashWith ":("
  Right i -> case P.runParser i inputParser of
    Left err -> unsafeCrashWith $ P.parseErrorMessage err
    Right o -> o

countRelevantP1 :: forall t. Array (Array t) -> Int
countRelevantP1 xs = xs <#> matches # sum
    where matches x = case A.length x of
                            2 -> 1
                            3 -> 1
                            4 -> 1
                            7 -> 1
                            _ -> 0

solve :: Input -> Effect Unit
solve i =
  let
    _ = spy "_" $ 0
  in
    do
      log "Day 08"
      log $ show $ i <#> (_.found >>> countRelevantP1) # sum

main :: Effect Unit
main = do
  FS.readTextFile UTF8 "input/d08.txt" (parse >>> solve)
