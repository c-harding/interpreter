module Parser.Combinators (
    -- * Parsing bytes
    char, satisfy, oneOf, noneOf,
    
    -- * Parsing sequences
    sepBy, sepBy1, chainl, chainl1, (<:>), string,
) where

import Control.Applicative (Alternative, empty, many, (<|>))
import Parser.Types        (Parser)
import Parser.BaseParsers  (item)

-- | Parse a char matching the predicate.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \t -> if p t then pure t else empty

-- | Parse exactly the given character.
char :: Char -> Parser Char
char c = satisfy (c ==)

-- | Parse any of the given characters.
oneOf :: [Char] -> Parser Char
oneOf = satisfy . flip elem

-- | Parse any character other than one given.
noneOf :: [Char] -> Parser Char
noneOf cs = satisfy (not . flip elem cs)

-- | Parse an exact string.
string :: String -> Parser String
string []     = return ""
string (c:cs) = char c <:> string cs

-- | Parse a list of items by @px@, separated with the separator @psep@. If
-- no items can be found, the empty list is returned.
sepBy  :: Alternative f => f a -> f sep -> f [a]
sepBy px psep = sepBy1 px psep <|> pure []

-- | Parse a list of items by @px@, separated with the separator @psep@. If
-- no items can be found, the parser fails.
sepBy1 :: Alternative f => f a -> f sep -> f [a]
sepBy1 px psep = px <:> (many (psep *> px))

-- | Cons a parsed value onto a parsed list.
(<:>) :: Applicative f => f a -> f [a] -> f [a]
px <:> pxs = (:) <$> px <*> pxs

-- | @chainl1 p op@ parses one or more occurrences of @p@, separated by
-- @op@. Returns a value produced by a /left/ associative application of all
-- functions returned by @op@.
chainl1 :: (Monad m, Alternative m) => m b -> m (b -> b -> b) -> m b
chainl1 p op = p >>= chainl p op

-- | @chainl p op x@ parses zero or more occurrences of @p@, separated by
-- @op@. Returns a value produced by a /left/ associative application of all
-- functions returned by @op@, starting with the value @x@. If there are no
-- occurrences of @p@, @x@ is returned.
chainl :: (Alternative m, Monad m) => m a -> m (b -> a -> b) -> b -> m b
chainl p op x = do
    f <- op
    y <- p
    chainl p op (f x y)
  <|> return x