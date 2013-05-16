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



renderGrammarLaTeX :: Grammar -> LaTeX
renderGrammarLaTeX = renderGrammar

-- | Transform a grammar to some LaTeX code.

renderGrammar :: LaTeXC l => Grammar -> l
renderGrammar (Grammar ps gname) = align xs where
  xs = [ (renderNtT l, mconcat (map renderNtT r)) | PR [l] r <- toList ps ]

-- | Transform a single terminal or non-terminal.

renderNtT :: LaTeXC l => NtT -> l
renderNtT = go
  where
    go (Nt d ns _) = ll <> nstex ns <> rr
    go (T  d ts _) = ll <> tstex ts <> rr
    ll = raw "\\begingroup \\left ( \\begin{smallmatrix}"
    rr = raw "\\end{smallmatrix} \\right ) \\endgroup" where
    special x
      | x == "epsilon" = epsilon
      | null x         = raw $ pack "-"
      | otherwise      = raw $ pack x
    tstex ts = mci [ special z | TSym z <- ts ]
    nstex ns = mci [ if m<=1 then special n
                             else special n !: (raw $ pack $ show i)
                   | NTSym n m i <- ns
                   ]

mci = mconcat . intersperse (raw "\\\\")

align :: LaTeXC l => [(l,l)] -> l
align = (liftL $ TeXEnv "align" []) . go where
  go xs = mci [ l & to <> r | (l,r) <- xs ]
