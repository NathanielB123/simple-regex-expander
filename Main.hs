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
import Data.List (foldl', nub, singleton, transpose)
import Data.List.NonEmpty (NonEmpty((:|)), (<|), fromList, tail, take, toList)
import Data.Set (member)
import Data.Set as Set (fromList, member)
import Data.Text (Text, pack)
import Data.Void (Void)
import GHC.Char (chr)
import GHC.Plugins (isSingleton)
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
  putStrLn
    (init . init $
     regexTargettedExpander Recurse 10 regex >>= (('"' :) . (++ "\", ")))

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
  flip Concatenation <$> regex

table :: [[Operator Parser Regex]]
table =
  [ [Postfix $ manyPostfixUnaryOp postfixOperators]
  , [InfixL (flatAlternation <$ token (char '|'))]
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

regexTargettedExpander :: AltStrategy -> Int -> Regex -> [String]
regexTargettedExpander as n r =
  Prelude.take n $
  snd $
  converge
    (\(l1, l1') (l2, _) -> lengthExceeds l1' n || l1 == l2)
    (fmap (\x -> (length x, x)) (regexExpander as r))

-- nub is innefficient here. Ideally should use some more efficient approach to
-- removedduplicates, but it needs to still be lazy.
regexExpander :: AltStrategy -> Regex -> [[String]]
regexExpander as regex = fmap (nub . flip (regexExpander' as) regex) nats

repeatList :: [a] -> Int -> [a]
repeatList l n = concat $ replicate n l

addParens :: String -> String
addParens = ('(' :) . (++ ")")

listLimit :: Int
listLimit = 3

applyIf :: (a -> a) -> Bool -> a -> a
applyIf f True = f
applyIf _ False = id

-- Current conclusions on "List" mode:
-- It works, but honestly isn't very clear
-- I think keeping "?" would be preferable to sometimes having (|a)
-- But nesting of choices is also a problem
-- Will have to think if there is a smarter way to represent
regexExpander' :: AltStrategy -> Int -> Regex -> [String]
regexExpander' Recurse n (FlatAlternation os) =
  Data.List.NonEmpty.take (n + 1) os >>= regexExpander' Recurse n
regexExpander' List n (FlatAlternation os) = tmp4
  where
    tmp1 = Data.List.NonEmpty.take listLimit os
    ellipses = lengthExceeds (Data.List.NonEmpty.tail os) $ listLimit - 1
    tmp2 = fmap (regexExpander' List n) tmp1
    tmp3 = transpose tmp2
    addEllipses = applyIf (++ "|..") ellipses
    tmp4 =
      fmap
        (\l ->
           applyIf addParens (not $ isSingleton l) $
           addEllipses $ init (l >>= (++ "|")))
        tmp3
regexExpander' as n (Concatenation r1 r2) =
  [a ++ b | a <- regexExpander' as n r1, b <- regexExpander' as n r2]
regexExpander' as n (Repetition r) =
  [0 .. n] >>= (\i -> fmap (`repeatList` i) (regexExpander' as n r))
regexExpander' _ n (Literal s) = [s]

data AltStrategy
  = Recurse
  | List
