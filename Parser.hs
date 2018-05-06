{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- This is a simple parser, based on the work of
-- [Nicolas Wu](http://zenzike.com/). It allows arbitrary backtracking, by
-- storing the possible parses as a list. I have adapted it to allow for
-- "partial parsing": It can report if an input is the start of a valid parse.

module Parser (
  -- * The Parser Monad
  Parser,

  -- * Running the parser
  parse,
  parseStart,
  parsePartial,

  -- ** Getting a list of possible parses
  listParse,
  listParseStart,
  listParsePartial,
  
  -- * Parsing bytes
  P.item, P.end,
  PC.char, PC.satisfy, PC.oneOf, PC.noneOf,

  -- * Lookahead
  P.ahead, P.peek,
  
  -- * Parsing sequences
  PC.sepBy, PC.sepBy1, PC.chainl, PC.chainl1, (PC.<:>), PC.string,
  ) where

import           Data.Foldable      (asum)
import           Data.Maybe         (isNothing, catMaybes)
import           Parser.Result      (Results, results, SingleResult, ListResult)
import           Parser.Types
import qualified Parser.BaseParsers as P
import qualified Parser.Combinators as PC

listParseStart :: Parser a -> String -> [(String, a)]
listParseStart px str = catMaybes $ runParser px (Closed, str)

parseStart :: Parser a -> String -> Maybe (String, a)
parseStart px str = asum $ runParser px (Closed, str)

listParse :: Parser a -> String -> [a]
listParse px = map snd . listParseStart (px <* P.end)

parse :: Parser a -> String -> Maybe a
parse px = fmap snd . parseStart (px <* P.end)

resultsParsePartial :: Results r => Parser a -> String -> r a
resultsParsePartial px str = results $ runParser (px <* P.end) (Open, str)

parsePartial :: Parser a -> String -> SingleResult a
parsePartial = resultsParsePartial

listParsePartial :: Parser a -> String -> ListResult a
listParsePartial = resultsParsePartial