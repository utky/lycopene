{ cabal, dlist, namesTh, persistableRecord
, sqlWords, text, time, timeLocaleCompat, transformers
}:

cabal.mkDerivation (self: {
  pname = "relational-query";
  version = "0.4.0.1";
  sha256 = "00ysy5lg0mpv5b1vcjlfi6nx72df3iqz5nmrfsrr0k7i65xp1fzi";
  buildDepends = [
    dlist namesTh persistableRecord sqlWords text time timeLocaleCompat
    transformers
  ];
  doCheck = false;
  meta = {
    homepage = "http://khibino.github.io/haskell-relational-record/";
    description = "Typeful, Modular, Relational, algebraic query engine";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
