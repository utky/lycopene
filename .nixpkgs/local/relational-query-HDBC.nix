{ cabal, convertible, HDBC, HDBCSession, namesTh, persistableRecord
, relationalQuery, relationalSchemas
}:

cabal.mkDerivation (self: {
  pname = "relational-query-HDBC";
  version = "0.1.0.0";
  sha256 = "1r5lj96w8cqcmma2kh46g8xyw0zz161nv1h9bwqia21vvis396vj";
  buildDepends = [
    convertible HDBC HDBCSession namesTh persistableRecord
    relationalQuery relationalSchemas
  ];
  meta = {
    homepage = "http://khibino.github.io/haskell-relational-record/";
    description = "HDBC instance of relational join and typed query for HDBC";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
