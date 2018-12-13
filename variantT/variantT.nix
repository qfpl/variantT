{ mkDerivation, base, bound, stdenv }:
mkDerivation {
  pname = "variantT";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base bound ];
  description = "Polymorphic variant for functor transformers";
  license = stdenv.lib.licenses.bsd3;
}
