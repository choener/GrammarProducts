
module BioInf.GrammarProducts.Op.Power where

import Control.Newtype
import Data.Semigroup
import Control.Lens
import Control.Lens.Fold
import qualified Data.Set as S
import Data.List (genericReplicate)
import Text.Printf

import BioInf.GrammarProducts.Grammar
import BioInf.GrammarProducts.Helper



-- |

power :: Grammar -> Integer -> Grammar
power gr k = Grammar (S.map f $ gr^.ps) (gr^.gname ++ "," ++ show k) where
  f (PR l r) = PR (map g l) (map g r)
  g (Nt d s) = Nt (d*k) (concat $ genericReplicate k s)
  g (T  d s) = T  (d*k) (concat $ genericReplicate k s)

