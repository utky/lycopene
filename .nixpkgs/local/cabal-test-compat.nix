{ cabal, Cabal, QuickCheck }:

cabal.mkDerivation (self: {
  pname = "cabal-test-compat";
  version = "0.2.0.0";
  sha256 = "15lxyrza1n9saac1awjx482gi7wq3sshqf4ich6k9xkfj464lrdq";
  buildDepends = [ Cabal QuickCheck ];
  meta = {
    homepage = "http://twitter.com/khibino/";
    description = "Compatibility interface of cabal test-suite";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
