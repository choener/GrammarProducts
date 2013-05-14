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
    { _dim  :: Int
    , _symN :: [NTSym]
    , _guid :: Int
    }
  | T
    { _dim  :: Int
    , _symT :: [[TSym]]
    , _guid :: Int
    }

data PR = PR
  { _lhs :: NonEmpty NtT
  , _rhs :: NonEmpty NtT
  }

data Grammar = Grammar
  { _ps  :: Set PR
  }

makeLenses ''Grammar
makeLenses ''PR
makeLenses ''NtT
makeLenses ''NTSym
