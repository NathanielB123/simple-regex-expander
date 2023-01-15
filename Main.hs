{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Main where

import Control.Applicative (Alternative, Applicative(liftA2))
import Control.Applicative.Combinators (some)
import Control.Monad (void)
import Control.Monad.Combinators.Expr
  ( Operator(InfixL, Postfix, Prefix)
  , makeExprParser
  )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Data.Char (ord)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (foldl', nub, singleton)
import Data.List.NonEmpty (NonEmpty((:|)), (<|), fromList, take)
import Data.Set (member)
import Data.Set as Set (fromList, member)
import Data.Text (Text, pack)
import Data.Void (Void)
import GHC.Char (chr)
import GHC.Utils.Misc (lengthExceeds)
import Text.Megaparsec
  ( MonadParsec(lookAhead, try)
  , Parsec
  , Token
  , Tokens
  , (<?>)
  , (<|>)
  , anySingle
  , between
  , eof
  , many
  , parseMaybe
  , parseTest
  , satisfy
  )
import Text.Megaparsec.Char (char, space, space1)
import Text.Megaparsec.Char.Lexer (symbol)

-- TODO:
-- Alternate way of displaying alternations to avoid exponential expansions
-- via picking different options.
-- Parsing escap characters ("\X")
-- Some actual string matching - NFA/DFA conversion?
main :: IO ()
main = do
  line <- liftIO getLine
  -- parseTest (regex <* eof) $ pack line
  regex <- maybeToM "Failed!" (parseMaybe (regex <* eof) $ pack line)
  putStrLn "Succeeded!\nPossible Matches:"
  putStrLn (regexTargettedExpander 10 regex >>= (('"' :) . (++ "\", ")))

maybeToM :: Monad m => [Char] -> Maybe a -> m a
maybeToM errMsg Nothing = error errMsg
maybeToM _ (Just a) = return a

-- print . parseMaybe (regex <* eof) $ pack line
type Parser = Parsec Void Text

-- Old idea for parsing - essentially would first parse into an AST losslessly
-- and then do a second pass on the AST converting it to the more constrained
-- subset of Regex
-- Then I realised (obviously, duh) the AST does not need to necessarily mirror
-- the parsing
{-
newtype Regex = Regex (RegexTree Regex)

data RegexTree a
    = Alternation a a
    | Concatenation a a
    | Repetition a
    | Literal String
    deriving (Show)

data ParseTree
    = CoreRegex (RegexTree ParseTree)
    | Optional ParseTree
    | NonZeroRep P
-}
-- Note alternations are parsed into a flat node, unlike concatenation
data Regex
  = FlatAlternation (NonEmpty Regex)
  | Concatenation Regex Regex
  | Repetition Regex
  | Literal String
  deriving (Show)

flatAlternation :: Regex -> Regex -> Regex
flatAlternation (FlatAlternation o) r = FlatAlternation $ r <| o
flatAlternation r1 r2 = FlatAlternation $ r1 :| [r2]

-- Not exported from Text.Megaparsec.Char??
identifierChar :: (MonadParsec e s m, Token s ~ Char) => m Char
identifierChar =
  satisfy (\c -> not $ c `member` Set.fromList " *()?+|") <?> "character"

-- manyUnaryOp adapted for prefix operators should not need to flip composition
-- around
manyPostfixUnaryOp :: Alternative f => f (a -> a) -> f (a -> a)
manyPostfixUnaryOp op = foldr1 (flip (.)) <$> some op

postfixOperators :: Parser (Regex -> Regex)
postfixOperators =
  Repetition <$ token (char '*') <|> nonZeroRep <$ token (char '+') <|>
  optionalReg <$ token (char '?') <|>
  Concatenation <$> regex

table :: [[Operator Parser Regex]]
table =
  [ [Postfix $ manyPostfixUnaryOp postfixOperators]
  , [ InfixL (flatAlternation <$ token (char '|'))
    -- , InfixL (Concatenation <$ try space1)
    ]
  ]

-- Converts a parser to one that consumes all spaces  before it before trying to
-- parse (typically done in the opposite order, but this makes parsing ' ' as
-- an operator easier)
-- The other option would be to parse regex as potentially as a concatenation to
-- the previous, maybe that is neater, I am unsure.
token :: Parser a -> Parser a
token p =
  try $ do
    a <- space
    p

emptyReg :: Regex
emptyReg = Literal ""

optionalReg :: Regex -> Regex
optionalReg = flatAlternation emptyReg

nonZeroRep :: Regex -> Regex
nonZeroRep r = Concatenation r $ Repetition r

parens :: Parser a -> Parser a
parens = between (token $ char '(') (token $ char ')')

term :: Parser Regex
term = parens regex <|> token range <|> token literal

regex :: Parser Regex
regex = makeExprParser term table

whitespace :: Parser ()
whitespace = void (many (char ' '))

range :: Parser Regex
range =
  try $ do
    fst <- token identifierChar
    token $ char '-'
    snd <- token identifierChar
    let fstOrd = ord fst
    let sndOrd = ord snd
    let os = map (Literal . singleton . chr) [ord fst .. ord snd]
    -- Will panick if empty, should ideally verify ranges are valid
    return . FlatAlternation $ Data.List.NonEmpty.fromList os

literal :: Parser Regex
literal = some identifierChar <&> Literal

nats :: [Int]
nats = iterate (+ 1) 0

converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
  | p x y = x
  | otherwise = converge p ys

regexTargettedExpander :: Int -> Regex -> [String]
regexTargettedExpander n r =
  Prelude.take n $
  snd $
  converge
    (\(l1, l1') (l2, _) -> lengthExceeds l1' n || l1 == l2)
    (fmap (\x -> (length x, x)) (regexExpander r))

-- Inefficient use of nub here, ideally should swap for some better duplicate
-- removal function, but needs to still be lazy
regexExpander :: Regex -> [[String]]
regexExpander regex = fmap (nub . (`regexExpander'` regex)) nats

repeatList :: [a] -> Int -> [a]
repeatList l n = concat $ replicate n l

regexExpander' :: Int -> Regex -> [String]
regexExpander' n (FlatAlternation os) =
  Data.List.NonEmpty.take (n + 1) os >>= regexExpander' n
regexExpander' n (Concatenation r1 r2) =
  [a ++ b | a <- regexExpander' n r1, b <- regexExpander' n r2]
regexExpander' n (Repetition r) =
  [0 .. n] >>= (\i -> fmap (`repeatList` i) (regexExpander' n r))
regexExpander' n (Literal s) = [s]
