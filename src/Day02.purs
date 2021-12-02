module Day02 where

import Prelude
import Effect.Console (log)
import Data.Either (Either(..))
import Node.FS.Async as FS
import Node.Encoding (Encoding(UTF8))
import Partial.Unsafe (unsafeCrashWith)
import Data.Maybe (Maybe(..))
import Data.String.Utils (lines, words)
import Data.Int (fromString)
import Data.Array (mapMaybe, foldl)

data Op
  = Forward Int
  | Down Int
  | Up Int

parse :: _
parse =
  let
    parseLine line = do
      { op, n } <- case words line of
        [ op, n ] -> Just { op, n }
        _ -> Nothing
      n <- fromString n
      case op of
        "forward" -> Just (Forward n)
        "down" -> Just (Down n)
        "up" -> Just (Up n)
        _ -> Nothing
  in
    case _ of
      Left _ -> unsafeCrashWith "Failed to read"
      Right buff -> lines buff # mapMaybe parseLine

solve :: _
solve i =
  let
    first =
      foldl
        ( \p o -> case o of
            Down n -> p { depth = p.depth + n }
            Up n -> p { depth = p.depth - n }
            Forward n -> p { pos = p.pos + n }
        )
        { depth: 0, pos: 0 }
        i

    second =
      foldl
        ( \p o -> case o of
            Down n -> p { aim = p.aim + n }
            Up n -> p { aim = p.aim - n }
            Forward n -> p { pos = p.pos + n, depth = p.depth + p.aim * n }
        )
        { depth: 0, pos: 0, aim: 0 }
        i
  in
    do
      log "Day 02"
      log $ show (first.depth * first.pos)
      log $ show (second.depth * second.pos)

main :: _
main = do
  FS.readTextFile UTF8 "inputd02.txt" (parse >>> solve)
