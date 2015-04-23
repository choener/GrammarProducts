{-# LANGUAGE FlexibleInstances #-}

module FormalLanguage.GrammarProduct.Op.Add where

import Control.Lens hiding (outside)
import Control.Lens.Fold
import Control.Newtype
import Data.List (genericReplicate)
import Data.Monoid hiding ((<>))
import Data.Semigroup
import qualified Data.Set as S
import Text.Printf
import Data.Default

import FormalLanguage.CFG.Grammar

import FormalLanguage.GrammarProduct.Op.Common



-- |

add :: Grammar -> Grammar -> Grammar
add l r = runAdd $ Add l <> Add r

-- | Add two grammars. Implemented as the union of production rules without any
-- renaming.

newtype Add a = Add {runAdd :: a}



-- | Note that the semigroup on Add will create a new rule S_gh -> S_g | S_h in
-- case two start symbols with different rhs exist (If S_g, S_h are the same,
-- there is no problem).

instance Semigroup (Add Grammar) where
  (Add l) <> (Add r)
    | Left err <- opCompatible l r = error err
    | otherwise = Add $ Grammar (l^.synvars  <> r^.synvars)
                                (l^.synterms <> r^.synterms) -- TODO add the newly created symbol to the non-terminals (or maybe just run ``fix T+N 's from the rules?'')
                                (l^.termvars <> r^.termvars)
                                (l^.outside)
                                (l^.rules <> r^.rules) -- 
                                s
                                (l^.params <> r^.params)
                                (l^.grammarName <> r^.grammarName)
                                False
    where s | l^.start == r^.start = l^.start
            | l^.start /= mempty && r^.start /= mempty = error "add new start symbol"
            | l^.start == mempty                       = r^.start
            |                       r^.start == mempty = l^.start

instance Monoid (Add Grammar) where
  mempty = Add def
  mappend = (<>)

-- idempotency is not made explicit here

