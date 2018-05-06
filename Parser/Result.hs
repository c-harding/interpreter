module Parser.Result (
  SingleResult(..),
  ListResult(..),
  Results,
  results,
) where

import            Data.Maybe (catMaybes, isNothing)
import            Parser.Types

-- | The result of a possibly partial parse, where only the first successful
-- parse is given, if present.
data SingleResult a
  -- | The result is not a valid parse, but it is a prefix of a valid parse.
  = SIncomplete
  -- | The parse was successful, and the first successful parse is given.
  | SParsed a
  -- | The result is not a valid parse.
  | SFailed
  deriving Show

-- | The result of a possibly partial parse, where every successful parse is
-- given, if present.
data ListResult a
  -- | The result is not a valid parse, but it is a prefix of a valid parse.
  = LIncomplete
  -- | The parse was successful, and every successful parse is given in a
  -- list.
  | LParsed [a]
  -- | The result is not a valid parse.
  | LFailed deriving Show

-- | A typeclass for all the ways to receive the result of a partial parse.
class Results r where
  -- | Produce a result of a partial parse.
  results :: Parses a -> r a

instance Results ListResult where
  results xs
    | null xs          = LFailed
    | all isNothing xs = LIncomplete
    | otherwise        = LParsed (snd <$> catMaybes xs)

instance Results SingleResult where
  results xs = case results xs of
    LFailed -> SFailed
    LIncomplete -> SIncomplete
    LParsed (xs:_) -> SParsed xs