
-- | This parser extends the @FormalLanguage.Parser@ parser of single- and
-- multi-dim grammars to accept grammar product definitions as well.
--
-- TODO display function names like this: <fun,fun,fun>

module FormalLanguage.GrammarProduct.Parser where

import Control.Arrow
import Control.Applicative
import Control.Lens
import Control.Monad (MonadPlus(..), guard, when)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Data.Default
import Data.Either
import Data.Map (Map)
import Data.Set (Set)
import Debug.Trace
import Data.List
import qualified Data.ByteString.Char8 as B
--import qualified Data.HashSet as H
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Parser.Expression
import Text.Parser.Token.Highlight
import Text.Parser.Token.Style
import Text.Printf
import Text.Trifecta
import Text.Trifecta.Delta
import Text.Trifecta.Result
import Data.Semigroup ((<>))
import qualified "newtype" Control.Newtype as T
--import Numeric.Natural.Internal
import Prelude hiding (subtract)
import Control.Monad
import Data.Char (isUpper)
import Data.Data.Lens
import System.IO.Unsafe (unsafePerformIO)

import FormalLanguage.CFG.Grammar
import FormalLanguage.CFG.Parser
import FormalLanguage.CFG.PrettyPrint.ANSI

import FormalLanguage.GrammarProduct.Op



-- | The top-level parser for a grammar product. It can be used as one of the
-- additional parser arguments, the formal grammars parser accepts.

parseGrammarProduct :: Parse m ()
parseGrammarProduct = do
  reserve fgIdents "Product:"
  n <- newGrammarName
  current <~ parseProductString
  current.grammarName .= n -- need to set here, otherwise the underlying combinator produces a combined name (funny but useless ;-)
  reserve fgIdents "//"
  v <- use verbose
  g <- use current
  seq (unsafePerformIO $ if v then (printDoc $ genGrammarDoc g) else return ())
    $ env %= M.insert n g



-- | Performs the actual parsing of a product string. Uses an expression parser
-- internally.

parseProductString :: Parse m Grammar
parseProductString = getGrammar <$> expr
  where expr :: Parse m ExprGrammar
        expr = buildExpressionParser table term
        table = [ [ binary "><"  exprDirect AssocLeft
                  , binary "*"   exprPower  AssocLeft
                  ]
                , [ binary "+"   exprPlus   AssocLeft
                  , binary "-"   exprMinus  AssocLeft
                  ]
                ]
        term =   parens expr
             <|> (ExprGrammar <$> knownGrammarName <?> "grammar not available in environment")
             <|> (ExprNumber  <$> natural <?> "integral power of grammar")
        binary n f a = Infix (f <$ reserve fgIdents n) a
        exprDirect l r = ExprGrammar (getGrammar l ><          getGrammar r)
        exprPlus   l r = ExprGrammar (getGrammar l `gAdd`      getGrammar r)
        exprMinus  l r = ExprGrammar (getGrammar l `gSubtract` getGrammar r)
        exprPower  l r = ExprGrammar (getGrammar l `gPower`    getNumber  r)

data ExprGrammar
  = ExprGrammar { getGrammar :: Grammar }
  | ExprNumber  { getNumber  :: Integer }

