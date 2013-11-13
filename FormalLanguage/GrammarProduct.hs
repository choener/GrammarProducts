
-- | This module contains the top-level functionality required to define
-- "products of grammars" (or more sloppily "how to multiply dynamic
-- programming algorithms"). Some operators (like '(><)') will check if both
-- grammars are compatible with the operation and fail if not.
--
-- TODO Later on we probably will be able to multiply without restrictions.

module FormalLanguage.GrammarProduct
  ( (><)
  ) where

import FormalLanguage.GrammarProduct.Op.Greibach as Greibach
import FormalLanguage.GrammarProduct.Op.Chomsky  as Chomsky
import FormalLanguage.GrammarProduct.Op.LinearDirect as Linear
import FormalLanguage.GrammarProduct.Op.Add



-- | The product of two grammars.
--
-- In general, it is quite hard to define the product of two context-free
-- grammars in a way that keeps associativity and also "does what we want it to
-- do" (see paper). For linear grammars it is much easier. Also, for grammars
-- in certain normal forms, a simpler definition is possible. Due to this, we
-- make the choice of the actual way on how to multiply based on the type of
-- grammars given. This, however, should only affect the resulting rules, not
-- the (multi-tape) language that the operations yields.
--
-- TODO I think, left-linear could reasonably be expanded to both, left- and
-- right-linear and maybe linear in general.
--
-- NOTE A proof for associativity is possible, but generally hard, so we prefer
-- to let the framework perform the proof for us.

(><) :: Grammar -> Grammar -> Grammar
g >< h
  | isLeftLinear g && isLeftLinear h = undefined
  | isChomskyNF  g && isChomskyNF  h = undefined
  | isGreibachNF g && isGreibachNF h = undefined
  | otherwise                        = error "Grammars in general CFG form are not handled. You need to convert into either Greibach- or Chomsky normal form. This might change in the future"

-- | The addition operation defined for two grammars of the same dimension. It
-- forms a monoid under the 'Add' newtype.

(.+) :: Grammar -> Grammar -> Grammar
g .+ h = runAdd $ Add g <> Agg h

