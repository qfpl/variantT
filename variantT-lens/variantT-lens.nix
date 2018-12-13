{ mkDerivation, base, lens, stdenv, variantT }:
mkDerivation {
  pname = "variantT-lens";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base lens variantT ];
  description = "Optics for @variantT@";
  license = stdenv.lib.licenses.bsd3;
}
