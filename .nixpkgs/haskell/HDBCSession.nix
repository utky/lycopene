{ cabal, HDBC }:

cabal.mkDerivation (self: {
  pname = "HDBC-session";
  version = "0.1.0.0";
  sha256 = "1fxx0q9hnxwsivsg2qinm0n3lvf89r9b72cnhipjlpf36nin5x5w";
  buildDepends = [ HDBC ];
  meta = {
    homepage = "http://khibino.github.io/haskell-relational-record/";
    description = "Bracketed connection for HDBC";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
