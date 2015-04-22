
module FormalLanguage.GrammarProduct.Op.Power where

--import Control.Newtype
import Data.Semigroup
import Control.Lens
import Control.Lens.Fold
import Data.Set.Lens
import qualified Data.Set as S
import Data.List (genericReplicate)
import Text.Printf

import FormalLanguage.CFG.Grammar



-- |

power :: Grammar -> Integer -> Grammar
power g k
  | k < 1     = error $ "Op.Power.power: power " ++ show k ++ " < 1"
  | otherwise = over (rules . setmapped . attr) kAttr
              . over (rules . setmapped . rhs . traverse) kDim
              . over (rules . setmapped . lhs) kDim
              . over start kDim
              $ g
  where kDim :: Symbol -> Symbol
        kDim = Symbol . concat . genericReplicate k . _getSymbolList
        kAttr :: [AttributeFunction] -> [AttributeFunction]
        kAttr = concat . genericReplicate k

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

