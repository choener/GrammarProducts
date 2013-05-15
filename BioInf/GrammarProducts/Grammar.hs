{-# LANGUAGE TemplateHaskell #-}

module BioInf.GrammarProducts.Grammar where

import Data.List.NonEmpty (NonEmpty)
import Data.Set (Set)
import Control.Lens
import Control.Lens.TH



data NTSym = NTSym
  { _name    :: String
  , _modulus :: Integer
  , _index   :: Integer
  }
  deriving (Show,Eq,Ord)

newtype TSym  = TSym  String
  deriving (Show,Eq,Ord)

data NtT
  = Nt
    { _dim  :: Integer
    , _symN :: [NTSym]
    , _guid :: Integer
    }
  | T
    { _dim  :: Integer
    , _symT :: [[TSym]]
    , _guid :: Integer
    }
  deriving (Show,Eq,Ord)

data PR = PR
  { _lhs :: NonEmpty NtT
  , _rhs :: NonEmpty NtT
  }
  deriving (Show,Eq,Ord)

data Grammar = Grammar
  { _ps    :: Set PR
  , _gname :: String
  }
  deriving (Show)

makeLenses ''Grammar
makeLenses ''PR
makeLenses ''NtT
makeLenses ''NTSym
