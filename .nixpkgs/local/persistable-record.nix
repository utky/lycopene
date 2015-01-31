{ cabal, dlist, namesTh, transformers }:

cabal.mkDerivation (self: {
  pname = "persistable-record";
  version = "0.1.0.0";
  sha256 = "1z03rixy03zp4l4ygb9jlj4p4x5vp20r5qq39hi8vn1x37j39x26";
  buildDepends = [ dlist namesTh transformers ];
  meta = {
    homepage = "http://khibino.github.io/haskell-relational-record/";
    description = "Binding between SQL database values and haskell records";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
