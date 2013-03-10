{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BioInf.GrammarProducts.Parser where

import Control.Applicative
import Text.Trifecta
import Text.Trifecta.Delta
import Text.Trifecta.Result
import Data.Either

import BioInf.GrammarProducts.Grammar



-- * parsers

-- | parse a single non-terminal

pN :: CharParsing f => f Sym
pN = (symN .) . (:) <$> upper <*> many alphaNum

-- | parse a single terminal. There is a special case of one or more '-' which
-- we may use too. Theses will probably be bound to a parser that doesn't
-- advance.

pT :: CharParsing f => f Sym
pT =   (symT .) . (:) <$> lower <*> many alphaNum
   <|> symT           <$> some (char '-')

-- | parse the list of non-terminal symbols

pNdefn :: CharParsing f => f [Sym]
pNdefn = string "N:" *> ws *> sepBy1 pN ws

-- | parse the list of terminal symbols

pTdefn :: CharParsing f => f [Sym]
pTdefn = string "T:" *> ws *> sepBy1 pT ws

-- | Functions

pFuns :: CharParsing f => f [VFun]
pFuns = string "F:" *> ws *> sepBy1 pFun ws

pFun = f <$> lower <*> many letter where
  f c cs = VFun [c:cs]

-- | Grammar name

pGrammarName = (:) <$> upper <*> some alphaNum

-- | Parse the first line of the grammar description

pHeader :: CharParsing f => f String
pHeader = oneOf "Gg" *> string "rammar:" *> ws *> pGrammarName

-- | Parse the last line of the grammar description

pLast = string "//"

pRules :: CharParsing f => f Rule
pRules = f <$> pN <* ws <* string "->" <* ws <*> pFun <* ws <* char '$' <* ws <*> (sepEndBy1 (pN <|> pT) wx) where
  f l f rs = Rule (VSym $ [l]) f [ VSym [r] | r <- rs ]



-- | Parse a full grammar description

pGrammar = do
  h <- pHeader <* newline
  nts <- sepEndBy1 (Left <$> pNdefn <|> Right <$> pTdefn) newline
  fs  <- sepEndBy1 pFuns newline
  rs <- sepEndBy1 pRules newline
  pLast
  let ns :: [VSym] = map (VSym . (:[])) $ concat $ lefts  nts
  let ts :: [VSym] = map (VSym . (:[])) $ concat $ rights nts
  let g = Grammar
            { name      = h
            , terminals = ts
            , nonterms  = ns
            , functions = concat fs
            , rules     = rs
            }
  return g

pProductOps = sepEndBy (pGr <|> pOp) ws where -- (:) <$> pGr <*> many (pOp <*> pGr) where
  pGr = ProdGr <$> pGrammarName
  pOp = ProdOp <$> string "*"

-- | parse what we maybe want to delete

pDelete = string "remove:" *> ws *> some pR where
  pR = (Left . VSym) <$> sepBy1 pT (char ',')

--pAddRule :: CharParsing f => f (Either a Rule)
pAddRule = (\c -> [Right c]) <$ string "addrule:" <* ws <*> pVRule

pVRule :: CharParsing f => f Rule
pVRule = f <$> pVN <* ws <* string "->" <* ws <*> pVF <* ws <* char '$' <* ws <*> (sepEndBy1 (pVN <|> pVT) wx) where
  f l f rs = Rule l f rs
  pVN = VSym <$> sepBy1 pN (char ',')
  pVT = VSym <$> sepBy1 pT (char ',')
  pVF = (\fs -> VFun [f | VFun [f] <- fs]) <$> sepBy1 pFun (char ',')

-- | Parse a product description

pProduct = do
  n <- (:) <$ string "Product:" <* ws <*> letter <*> some alphaNum <* newline
  p <- string "Prod:" *> ws *> pProductOps <* newline
  rs <- sepEndBy1 (pDelete <|> pAddRule) newline
  let ds = lefts $ concat rs
  let rules = rights $ concat rs
  pLast
  return $ GProduct
    { pname = n
    , pprod = p
    , pdels = ds
    , prules = rules
    }
--  return (n,p,rs)

pFullDesc = do
  gs <- sepEndBy1 pGrammar newline <?> "need to define at least one grammar!"
  p  <- pProduct <?> "need to define exactly one product description"
  spaces
  eof
  return (gs,p)

-- | helper function with 1 .. space characters

ws = some $ char ' '
wx = many $ char ' '

