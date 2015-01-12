{ cabal, persistableRecord, relationalQuery, time }:

cabal.mkDerivation (self: {
  pname = "relational-schemas";
  version = "0.1.0.1";
  sha256 = "15fgh42j2fhgqxr2z6ayg7kwrdmsmakl5v1dkxyrp7k3iah3ak5d";
  buildDepends = [ persistableRecord relationalQuery time ];
  meta = {
    homepage = "http://khibino.github.io/haskell-relational-record/";
    description = "RDBMSs' schema templates for relational-query";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
