{ cabal }:

cabal.mkDerivation (self: {
  pname = "names-th";
  version = "0.1.0.0";
  sha256 = "05ghdjif8r0x8k2qqixh2wzzq356f80py1nfps6wim8g9sv1h5rc";
  meta = {
    homepage = "http://khibino.github.io/haskell-relational-record/";
    description = "Manipulate name strings for TH";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
