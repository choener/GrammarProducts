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



instance Semigroup (Add Grammar) where
  (Add l) <> (Add r)
    | gDim l /= gDim r = error $ printf "ERROR: grammars \n%s\n and \n%s\n have different dimensions, cannot unify." (show l) (show r)
    | otherwise = Add $ Grammar (S.union (l^.tsyms) (r^.tsyms))
                                (S.union (l^.nsyms) (r^.nsyms))
                                (S.union (l^.rules) (r^.rules))
                                (if l^.start == r^.start
                                  then l^.start
                                  else error "maybe add another rule and a unique start symbol?")

instance Monoid (Add Grammar) where
  mempty = Add $ Grammar (S.empty) (S.empty) (S.empty) (Symb [])
  mappend = (<>)

-- idempotency is not made explicit here

