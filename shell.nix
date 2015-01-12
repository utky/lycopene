let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages_ghc763.override {
    extension = self: super: {
      thispkg = self.callPackage ./. {};
      HDBCSession = self.callPackage ./.nixpkgs/haskell/HDBC-session.nix {};
      namesTh = self.callPackage ./.nixpkgs/haskell/names-th.nix {};
      relationalRecord = self.callPackage .nixpkgs/haskell/relational-record.nix {};
      relationalQuery = self.callPackage .nixpkgs/haskell/relational-query.nix {};
      relationalQueryHDBC = self.callPackage .nixpkgs/haskell/relational-query-HDBC.nix {};
      persistableRecord = self.callPackage .nixpkgs/haskell/persistable-record.nix {};
      cabalTestCompat = self.callPackage .nixpkgs/haskell/cabal-test-compat.nix {};
      sqlWords = self.callPackage .nixpkgs/haskell/sql-words.nix {};
      timeLocaleCompat = self.callPackage .nixpkgs/haskell/time-locale-compat.nix {};
      relationalSchemas = self.callPackage .nixpkgs/haskell/relational-schemas.nix {};
    };
  };
in pkgs.myEnvFun {
     name = haskellPackages.thispkg.name;
     buildInputs = [
       (haskellPackages.ghcWithPackages (hs: ([
         hs.cabalInstall
         hs.hscolour
       ] ++ hs.thispkg.propagatedNativeBuildInputs)))
     ];
   }      
