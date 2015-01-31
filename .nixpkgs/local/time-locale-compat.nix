{ cabal, time }:

cabal.mkDerivation (self: {
  pname = "time-locale-compat";
  version = "0.1.0.1";
  sha256 = "0q5d134cvcy7hlr473fanqqixqnqpqvz9ka2r45m59l6kzrws95c";
  buildDepends = [ time ];
  meta = {
    homepage = "http://twitter.com/khibino/";
    description = "Compatibility of TimeLocale between old-locale and time-1.5";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
