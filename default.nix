{ mkDerivation, ADPfusion, base, bytestring, containers
, data-default, FormalGrammars, lens, lib, newtype, parsers
, PrimitiveArray, semigroups, template-haskell, transformers
, trifecta
}:
mkDerivation {
  pname = "GrammarProducts";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ADPfusion base bytestring containers data-default FormalGrammars
    lens newtype parsers PrimitiveArray semigroups template-haskell
    transformers trifecta
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/choener/GrammarProducts";
  description = "Grammar products and higher-dimensional grammars";
  license = lib.licenses.gpl3Only;
}
