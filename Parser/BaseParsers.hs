{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Parser.BaseParsers (
  item,
  ahead,
  peek,
  end,
) where

import Parser.Types (Termination(..), Parser(..), runParser)

-- | Match any character.

item :: Parser Char
item = Parser $ \case
  (Open,   [])   -> [Nothing]
  (Closed, [])   -> []
  (_,    t:ts)   -> [Just (ts, t)]

-- | Run a parser, then reset the input (lookahead).
ahead :: Parser a -> Parser a
ahead px = Parser $ \(term, str) ->
  fmap ((str,) . snd) <$> runParser px (term, str)

-- | Match any character with lookahead.
peek :: Parser Char
peek = ahead item

-- | Match the end of the input. Note that this also matches at the end of the
-- current input block when partial parsing is enabled, if further parsers are
-- chained, the result is @Incomplete@, instead of @Failed@.

end :: Parser ()
end = Parser $ \case
  (_, []) -> [Just ("", ())]
  _       -> []