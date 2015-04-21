
module FormalLanguage.GrammarProduct.Op.Power where

import Control.Newtype
import Data.Semigroup
import Control.Lens
import Control.Lens.Fold
import qualified Data.Set as S
import Data.List (genericReplicate)
import Text.Printf

import FormalLanguage.CFG.Grammar



-- |

power :: Grammar -> Integer -> Grammar
power g k
  | k < 1     = error $ "Op.Power.power: power " ++ show k ++ " < 1"
  | otherwise = over (lens) (what) g
  where kDim :: Symbol -> Symbol
        kDim = undefined
        kAttr :: AttributeFunction -> AttributeFunction
        kAttr = undefined
{-
power g k = Grammar ts ns es rs s nm where
  ts = g^.tsyms
  ns = S.map go $ g^.nsyms
  es = g^.epsis
  rs = S.map (\(Rule l f rs) -> Rule (go l) (f++f) (map go rs)) $ g^.rules
  s  = fmap go $ g^.start
  nm = concat . genericReplicate k $ g^.name
  go (Symb z) = Symb . concat $ genericReplicate k z
-}

