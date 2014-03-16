
-- | This module contains the top-level functionality required to define
-- "products of grammars" (or more sloppily "how to multiply dynamic
-- programming algorithms"). Some operators (like '(><)') will check if both
-- grammars are compatible with the operation and fail if not.
--
-- TODO Later on we probably will be able to multiply without restrictions.

module FormalLanguage.GrammarProduct
  ( module FormalLanguage.GrammarProduct.QQ
  ) where

{-
-}
import FormalLanguage.GrammarProduct.QQ (grammarProduct)

