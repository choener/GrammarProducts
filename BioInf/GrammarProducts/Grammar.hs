{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module BioInf.GrammarProducts.Grammar where

import Control.Lens
import Data.Data
import Data.Data.Lens
import Data.Typeable
import Data.List



-- ** grammar data types

data SymType
  = N
  | T
  deriving (Eq,Ord,Show,Data,Typeable)

-- | A symbol is a single terminal or non-terminal on either
-- left-hand or right-hand side. This is, on the level of
-- symbol we do not distinguish between LHS and RHS (or
-- equivalently the type of grammar we are looking at).

data Sym = Sym
  { _t :: SymType
  , _n :: String
  } deriving (Eq,Ord,Show,Data,Typeable)

makeLenses ''Sym

symN = Sym N
symT = Sym T

newtype VSym = VSym [Sym]
  deriving (Eq,Ord,Data,Typeable)

instance Show VSym where
  show (VSym []) = ""
  show (VSym xs@(Sym t n:_)) = show t ++ ":" ++ (concat . intersperse "." . map _n $ xs)

-- | multi-dimensional function

newtype VFun = VFun [String]
  deriving (Eq,Ord,Show,Data,Typeable)

-- | full production rule

data Rule = Rule
  { _lhs :: VSym
  , _fun :: VFun
  , _rhs :: [VSym]
  }
  deriving (Eq,Ord,Show,Data,Typeable)

-- | full grammar

data Grammar = Grammar
  { name      :: String
  , terminals :: [VSym]
  , nonterms  :: [VSym]
  , functions :: [VFun]
  , rules     :: [Rule]
  }
  deriving (Eq,Ord,Show,Data,Typeable)

-- | A grammar product definition

data GProduct = GProduct
  { pname :: String
  , pprod :: [ProdOps]
  , pdels :: [VSym]
  }
  deriving (Eq,Ord,Show,Data,Typeable)

-- | Parse product operations

data ProdOps
  = ProdGr String
  | ProdOp String
  deriving (Eq,Ord,Show,Data,Typeable)

-- | No types, since we want this to work on multiple levels

symType xs
  | allOf biplate (==T) xs = T
  | allOf biplate (==N) xs = N
  | otherwise = error $ "mixed symbol type found in: " ++ show xs
