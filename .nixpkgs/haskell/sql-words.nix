{ cabal, Cabal, cabalTestCompat, QuickCheck }:

cabal.mkDerivation (self: {
  pname = "sql-words";
  version = "0.1.3.0";
  sha256 = "03r6lk4rgk55idxqifazrq673hnkzr089b34qjv1x3yk160xnmz8";
  testDepends = [ Cabal cabalTestCompat QuickCheck ];
  meta = {
    homepage = "http://khibino.github.io/haskell-relational-record/";
    description = "Simple idea SQL keywords data constructor into OverloadedString";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
