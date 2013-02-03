{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}

module BioInf.GrammarProducts where

import Language.Haskell.TH.Quote
import qualified Language.Haskell.TH as TH
import Text.Trifecta
import Control.Applicative
import Data.Semigroup



-- | Grammars are represented much like in formal theory as a tuple. We
-- actually use attribute grammars (Knuth, 1968) to add algebras for
-- evaluation.
--
-- TODO we want to separate N/T in the long run ...

data Grammar = Grammar
  { nonterms    :: [NT]
  , terminals   :: [NT]
  , productions :: [Production]
  , startSym    :: NT
  }

-- | Terminals and non-terminals are the same species.

data NT where
  Terminal :: String -> NT
  Nonterm  :: String -> NT
  deriving (Show,Eq)

data Production where
  Production :: NT -> [[NT]] -> Production
  deriving (Show)

-- | productions should form a semigroup given equal NT identifier.
-- Unfortunately, we are not dependently-typed on nt ...

{-
instance Semigroup Production where
  (Production a xs) <> (Production b ys)
    | a==b = Production a (xs ++ ys)
    | otherwise = error ""
-}

grammar :: QuasiQuoter
grammar = QuasiQuoter
  { quoteExp = grammarExp
  }

grammarExp :: String -> TH.ExpQ
grammarExp = fail

data CP where
  CP :: [NT] -> [[NT]] -> CP
  deriving (Show)

{-
p2cp :: Production -> CP
p2cp (Production x ys) = CP [x] [ys]

cp :: CP -> CP -> CP
cp (CP a xs) (CP b ys) = CP (a++b) [ x++y | x<-xs, y<- ys ]

t = p2cp $ Production (Nonterm "X") [Nonterm "X", Terminal "u"]
-}

pN = Nonterm <$> some upper
pT = Terminal <$> some lower
--pP = Production <$> pN <* string " -> " <*> sepBy1 (pN <|> pT) ws
--pPs = foldl (flip (:)) [] <$> sepBy1 pP newline

ws = some $ char ' '
