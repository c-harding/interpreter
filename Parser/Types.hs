module Parser.Types (
  Termination(..),
  Input(..),
  Parses,
  Parser(..),
) where

import Control.Applicative (Applicative, Alternative, empty, (<|>), many)

-- | Termination refers to whether the input is known to terminate, or could
-- | be continued, e.g. from a REPL.

data Termination = Closed | Open

-- | Input is passed around internally as a tuple of the termination type and
-- | and the flag.

type Input = (Termination, String)

-- | The result of a parse is internally stored as a list of optional tuples
-- | of the remaining string and the parsed object. A value of Nothing within
-- | the list refers to a partial parse: the string is the prefix of a valid
-- | parse. This is, however, not returned to the user unless there are no
-- | valid complete parses.
type Parses a = [Maybe (String, a)]

-- | Perform an operation similar to monadic bind, producing parses out of the
-- result of a single parse
pbind :: Parses a -> ((String, a) -> Parses b) -> Parses b
pbind xs f = xs >>= maybe [Nothing] f

-- | A parser is simply a wrapper around a function transforming input to a
-- | list of valid 'Parses'.
newtype Parser a = Parser { runParser :: Input -> Parses a }

-- # Functor

-- The functor instance captures the idea of modifying the output of
-- successful parses.

instance Functor Parser where
--fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser px) = Parser $ \input -> fmap (fmap f) <$> px input


-- # Applicative

-- The applicative instance shows how parsers can be chained together.

instance Applicative Parser where
--pure :: a -> Parser a
  pure x = Parser $ \(_, str) -> [Just (str, x)]

--(<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser pf <*> Parser px = Parser $ \(term, str) -> pf (term, str) `pbind` \(str', f) -> fmap (fmap f) <$> px (term, str')


-- # Alternative

-- Choices between parsers are given by the @Alternative@ class. This
-- class assumes that the given Parser is already @Applicative@.

instance Alternative Parser where
--empty :: Parser a
  empty = Parser (\ts -> [])

--(<|>) :: Parser a -> Parser a -> Parser a
  Parser px <|> Parser py = Parser (\ts -> px ts <> py ts)


-- # Monad

-- The monad instance allows the value in the result of one parser to
-- influence the output of the parse.

instance Monad Parser where
--return :: a -> Parser a
  return = pure

--(>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser px >>= f = Parser (\(term, str) -> (px (term, str)) `pbind` \(str', x) -> runParser (f x) (term, str'))