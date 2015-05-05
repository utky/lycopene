with (import <nixpkgs> {}).pkgs;
let 
    haskellngPackages' = haskellngPackages.override {
      overrides = self: super: {
        sql-words = super.sql-words.override {
          mkDerivation = (attrs: self.mkDerivation (attrs // { doCheck = false; }));
        };
        relational-query = super.relational-query.override {
          mkDerivation = (attrs: self.mkDerivation (attrs // { doCheck = false; }));
        };
      };
    };
    pkg = haskellngPackages'.callPackage
            ({ mkDerivation, base, bytestring, directory, filepath, HDBC
             , HDBC-session, HDBC-sqlite3, hspec, mtl, optparse-applicative
             , persistable-record, pipes, pretty, relational-query
             , relational-query-HDBC, relational-record, stdenv
             , template-haskell, text, time, transformers
             }:
             mkDerivation {
               pname = "lycopene";
               version = "0.1.0.0";
               src = ./.;
               isLibrary = true;
               isExecutable = true;
               buildDepends = [
                 base bytestring directory filepath HDBC HDBC-session HDBC-sqlite3
                 mtl optparse-applicative persistable-record pipes pretty
                 relational-query relational-query-HDBC relational-record
                 template-haskell text time transformers
               ];
               testDepends = [ base hspec ];
               description = "A command line tool which provides pomodoro techniques";
               license = stdenv.lib.licenses.asl20;
             }) {};
in
  pkg.env
