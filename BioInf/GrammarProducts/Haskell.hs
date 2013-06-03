{-# LANGUAGE RecordWildCards #-}

-- | Render a grammar into a String, which may form the nucleus of a Haskell
-- module.
--
-- Note that the resulting file requires manual additions, such as the module
-- name.

module BioInf.GrammarProducts.Haskell where

import Data.Set (toList)
import Data.Function
import Control.Lens
import Data.List
import Text.Printf
import Data.Char

import BioInf.GrammarProducts.Grammar



renderGrammarHaskell :: [Grammar] -> String
renderGrammarHaskell gs = concatMap show gs

-- render single grammar

rgh :: Grammar -> String
rgh g = show g

-- render just the rules as a multidim Haskell grammar
--
-- TODO explicitly annotate the @""@ terminal symbol as the 'None' terminal
-- parser.

hsGrammar :: Grammar -> String
hsGrammar g = hdr ++ (concat $ inlined 2 [ "(", concat $ intersperse i ts, ")" ]) where
  hdr = printf "g%s S%s{..} {-non-terminals:-} %s {-terminals:-} %s\n" (g^.gname) (g^.gname) nbs tbs
  nbs = concat . intersperse " " . nub
      . concatMap (map mkNtSym . (^.lhs)) . toList $ g^.ps -- non-terminal binders
  tbs = concat . intersperse " " . filter (not . null)
      . nub . map (^.tname) . concatMap (^.symT) . filter isT
      . concatMap (^.rhs) . toList $ g^.ps -- terminal binders
  i = ""
  ts = []
  xs = groupBy ((==) `on` (^.lhs)) . toList $ g^.ps

mkNtSym :: NtT -> String
mkNtSym t@T{..} = error $ "dying at finding terminal symbol in mkNtSym: " ++ show t
mkNtSym (Nt _ ns) = concatMap (\(NTSym n _ i) -> printf "%s%d" (lowerHead n) i) ns

lowerHead [] = []
lowerHead (x:xs) = toLower x : xs

inlined k xs = map (replicate k ' ' ++) xs

-- TODO specialized renderer for single-dim grammars

-- render the signature

-- render algebra product
