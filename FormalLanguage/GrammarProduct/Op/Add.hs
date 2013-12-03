{-# LANGUAGE FlexibleInstances #-}

module FormalLanguage.GrammarProduct.Op.Add where

import Control.Lens
import Control.Lens.Fold
import Control.Newtype
import Data.List (genericReplicate)
import Data.Monoid hiding ((<>))
import Data.Semigroup
import qualified Data.Set as S
import Text.Printf

import FormalLanguage.Grammar



-- | Add two grammars. Implemented as the union of production rules without any
-- renaming.

newtype Add a = Add {runAdd :: a}



-- | Note that the semigroup on Add will create a new rule S_gh -> S_g | S_h in
-- case two start symbols with different rhs exist (If S_g, S_h are the same,
-- there is no problem).

instance Semigroup (Add Grammar) where
  (Add l) <> (Add r)
    | gDim l /= gDim r = error $ printf "ERROR: grammars \n%s\n and \n%s\n have different dimensions, cannot unify." (show l) (show r)
    | otherwise = Add $ Grammar (l^.tsyms <> r^.tsyms)
                                (l^.nsyms <> r^.nsyms) -- TODO add the newly created symbol to the non-terminals (or maybe just run ``fix T+N 's from the rules?'')
                                (l^.epsis <> r^.epsis)
                                (l^.rules <> r^.rules <> t)
                                s
                                (l^.name  <> r^.name)
    where s = case (l^.start,r^.start) of
                (Nothing, Nothing) -> Nothing
                (Nothing, Just k ) -> Just k
                (Just k , Nothing) -> Just k
                (Just k , Just l ) -> if k==l then Just k else error "need to create new symbol, see note on Semigroup (Add Grammar)"
          t = case (l^.start,r^.start) of
                (Just k , Just l ) -> if k==l then S.empty else error "this will create the new rule"
                _                  -> S.empty
                                --(if l^.start == r^.start
                                --  then l^.start
                                --  else error "maybe add another rule and a unique start symbol?")

instance Monoid (Add Grammar) where
  mempty = Add $ Grammar S.empty S.empty S.empty S.empty Nothing ""
  mappend = (<>)

-- idempotency is not made explicit here

