{ cabal, filepath, HDBC, HDBCSession, HDBCSqlite3, hspec, mtl
, optparseApplicative, persistableRecord, relationalQuery
, relationalQueryHDBC, relationalRecord, text, time, transformers
}:

cabal.mkDerivation (self: {
  pname = "lycopene";
  version = "0.1.0.0";
  sha256 = "nil";
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    filepath HDBC HDBCSession HDBCSqlite3 mtl optparseApplicative
    persistableRecord relationalQuery relationalQueryHDBC
    relationalRecord text time transformers
  ];
  testDepends = [ hspec ];
  meta = {
    description = "A command line tool which provides pomodoro techniques";
    license = self.stdenv.lib.licenses.asl20;
    platforms = self.ghc.meta.platforms;
  };
})
