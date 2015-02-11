let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self : super : rec {
      thisPackage = self.callPackage ./. {};

      HDBCSession = self.callPackage ./.nixpkgs/local/HDBC-session.nix {};
      namesTh = self.callPackage ./.nixpkgs/local/names-th.nix {};
      relationalRecord = self.callPackage ./.nixpkgs/local/relational-record.nix {};
      relationalQuery = self.callPackage ./.nixpkgs/local/relational-query.nix {};
      relationalQueryHDBC = self.callPackage ./.nixpkgs/local/relational-query-HDBC.nix {};
      persistableRecord = self.callPackage ./.nixpkgs/local/persistable-record.nix {};
      cabalTestCompat = self.callPackage ./.nixpkgs/local/cabal-test-compat.nix {};
      sqlWords = self.callPackage ./.nixpkgs/local/sql-words.nix {};
      timeLocaleCompat = self.callPackage ./.nixpkgs/local/time-locale-compat.nix {};
      relationalSchemas = self.callPackage ./.nixpkgs/local/relational-schemas.nix {};
    };
  };
in pkgs.myEnvFun {
  name = haskellPackages.thisPackage.name;
  buildInputs = [
    haskellPackages.cabalInstall
    haskellPackages.hlint
    haskellPackages.ghcMod
    haskellPackages.hasktags
    (haskellPackages.ghcWithPackages (hs: haskellPackages.thisPackage.propagatedNativeBuildInputs))
  ];
}

