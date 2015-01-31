{ cabal, relationalQuery, relationalQueryHDBC }:

cabal.mkDerivation (self: {
  pname = "relational-record";
  version = "0.1.1.0";
  sha256 = "1rxwlp49a1i4y2qnzjwjwp92ggphd92ikcgxpmkiw6a6frspy3zs";
  buildDepends = [ relationalQuery relationalQueryHDBC ];
  meta = {
    homepage = "http://khibino.github.io/haskell-relational-record/";
    description = "Meta package of Relational Record";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
