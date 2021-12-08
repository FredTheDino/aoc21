module Day08 where

import Prelude
import Data.Array (fromFoldable, (!!), (:))
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Map as M
import Data.Set as S
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

wordParser :: P.Parser String (Digit)
wordParser = do
  PS.skipSpaces
  letters <- PC.many1 PT.letter
  pure $ S.fromFoldable letters

type Line =
  { every :: Array Digit
  , found :: Array Digit
  }

type Digit = (S.Set Char)
type Knowledge = Array (Tuple (Digit) (Maybe Int))

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

countSimple xs = xs <#> matches # sum
  where
  matches x = case S.size x of
    2 -> 1
    3 -> 1
    4 -> 1
    7 -> 1
    _ -> 0

solveSimple :: Digit -> Maybe Int
solveSimple x =
  case S.size x of
    2 -> Just 1
    3 -> Just 7
    4 -> Just 4
    7 -> Just 8
    _ -> Nothing

combine as bs = A.zip as bs <#> merge
  where
  merge (Tuple (Tuple x a) (Tuple _ b)) =
    ( Tuple x case a, b of
        Nothing, Just _ -> b
        _, _ -> a
    )

isA :: forall t. Int -> (Tuple t (Maybe Int) -> Boolean)
isA n = \(Tuple _ x) -> x == Just n

solve960 :: Knowledge -> Digit -> Maybe Int
solve960 known x = do
  _ <- case S.size x of
    6 -> Just unit
    _ -> Nothing
  Tuple four _ <- A.find (isA 4) known
  Tuple seven _ <- A.find (isA 7) known
  if S.subset four x then Just 9
  else if S.subset seven x then Just 0
  else Just 6

solve235 :: Knowledge -> Digit -> Maybe Int
solve235 known x = do
  _ <- case S.size x of
    5 -> Just unit
    _ -> Nothing
  Tuple one _ <- A.find (isA 1) known
  Tuple four _ <- A.find (isA 4) known
  Tuple eight _ <- A.find (isA 8) known
  if S.subset one x then Just 3
  else if S.subset (S.difference eight four) x then Just 2
  else Just 5

figureOutWires :: Array Digit -> Knowledge
figureOutWires x =
  let
    apply f = A.zip x $ (x <#> f)
    step f a = combine a (apply (f a))
  in
    apply solveSimple
        # step solve960
        # step solve235
        
translate :: Tuple Knowledge (Array Digit) -> Array Int
translate (Tuple known xs) = xs
    <#> (\x -> A.findMap (\(Tuple a b) -> if a == x then b else Nothing) known)
    <#> fromMaybe 0

parseDec :: Array Int -> Int
parseDec = A.foldl (\i s -> 10 * i + s) 0

solve :: Input -> Effect Unit
solve i =
  let
    known = i <#> _.every <#> figureOutWires

    total = i
        <#> _.found # A.zip known
        <#> translate >>> parseDec
        # sum
    -- TODO, find the numbers (len and match), parse numbers
  in
    do
      log "Day 08"
      log $ show $ i <#> (_.found >>> countSimple) # sum
      log $ show $ total


main :: Effect Unit
main = do
  FS.readTextFile UTF8 "input/d08.txt" (parse >>> solve)
