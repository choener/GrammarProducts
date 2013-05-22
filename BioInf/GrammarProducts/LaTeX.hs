{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module BioInf.GrammarProducts.LaTeX where

import Text.LaTeX.Base
import Text.LaTeX.Base.Syntax
import Text.LaTeX.Base.Class
import Text.LaTeX.Packages.AMSMath
import Data.Text (pack)
import Data.Set (toList)
import Data.List (intersperse)

import BioInf.GrammarProducts.Grammar



renderGrammarLaTeX :: Int -> Grammar -> LaTeX
renderGrammarLaTeX = renderGrammar

-- | Transform a grammar to some LaTeX code.

renderGrammar :: LaTeXC l => Int -> Grammar -> l
renderGrammar k (Grammar ps gname)
  | k == 1 = align xs
  | k == 2 = align2 xs
  where -- subsubsection (raw $ pack gname) <> raw "\n" <> align2 xs <> raw "\n" where
    xs = [ (renderNtT l, mconcat (map renderNtT r)) | PR [l] r <- toList ps ]

-- | Transform a single terminal or non-terminal.

renderNtT :: LaTeXC l => NtT -> l
renderNtT = go
  where
    go (Nt d ns) = ll <> nstex ns <> rr
    go (T  d ts) = ll <> tstex ts <> rr
    ll = raw "\\begingroup \\left ( \\begin{smallmatrix}"
    rr = raw "\\end{smallmatrix} \\right ) \\endgroup" where
    special x
      | x == "empty" = varepsilon
      | null x       = epsilon -- raw $ pack "-"
      | otherwise    = raw $ pack x
    tstex ts = mci [ special z | TSym z <- ts ]
    nstex ns = mci [ if m<=1 then special n
                             else special n !: (raw $ pack $ show i)
                   | NTSym n m i <- ns
                   ]

mci = mconcat . intersperse (raw "\\\\\n")

align :: LaTeXC l => [(l,l)] -> l
align = (liftL $ TeXEnv "align*" []) . go where
  go xs = mci [ l & to <> r | (l,r) <- xs ]

align2 :: LaTeXC l => [(l,l)] -> l
align2 = (liftL $ TeXEnv "align*" []) . go where
  go xs = let len     = length xs
              (as,bs) = splitAt ((len +1) `div` 2) $ xs ++ repeat ("","")
              to' c = if c > len `div` 2 then "" else to
          in
              mci [ ll & to <> lr & rl & to' c <> rr | (ll,lr) <- as | ((rl,rr),c) <- zip bs [1..] ]

