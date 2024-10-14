{
  description = "hasql-generator";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-24.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    tmp-postgres.url = "github:tstat/tmp-postgres";
  };


  outputs = inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              hasql-generator = hfinal.callCabal2nix "hasql-generator" ./. { };
            };
        };
        hasql-generator = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.hasql-generator;
      };
      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [overlay]; };
          unstablePkgs = import inputs.nixpkgs-unstable { inherit system; overlays = [ overlay ]; };

          hspkgs = unstablePkgs.haskellPackages;

          tmp-postgres = inputs.tmp-postgres.packages;
        in
        {
          devShell = pkgs.haskell.packages.ghc98.shellFor {
            withHoogle = true;
            packages = p: [ p.hasql-generator ];
            buildInputs = [
              # Shell

              ## Bash (for interactive use)
              pkgs.bashInteractive


              # Database

              ## PostgreSQL
              pkgs.postgresql_16

              ## Easily start a temporary PostgreSQL server
              tmp-postgres."${system}".default


              # Haskell Tooling

              ## CLI interface for Cabal
              hspkgs.cabal-install

              ## Haskell formatter
              hspkgs.fourmolu

              ## ghciwatch
              unstablePkgs.ghciwatch

              ## HLS
              (pkgs.haskell-language-server.override { supportedGhcVersions = [ "982" ]; })

              ## HLint
              hspkgs.hlint

              ## hpack (to avoid needing to generate .cabal files)
              hspkgs.hpack


              # Dev UX

              ## https://pre-commit.com/
              pkgs.pre-commit
            ];
          };

          defaultPackage = pkgs.hasql-generator;
        };
    in
    { inherit overlay; } // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
