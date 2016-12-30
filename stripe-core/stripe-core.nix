{ mkDerivation, aeson, base, bytestring, mtl, stdenv, text, time
, transformers, unordered-containers, data-default-class
}:
mkDerivation {
  pname = "stripe-core";
  version = "2.0.1";
  src = ./.;
  buildDepends = [
    aeson base bytestring mtl text time transformers
    unordered-containers data-default-class
  ];
  homepage = "https://github.com/dmjio/stripe-haskell";
  description = "Stripe API for Haskell - Pure Core";
  license = stdenv.lib.licenses.mit;
}
