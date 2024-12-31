{
  description = "hasql-generator";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-24.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    tmp-postgres.url = "github:tstat/tmp-postgres";
    hs_pg-query.url = "git+https://git.sr.ht/~rhizomic/pg-query";
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system: let
      pkgsWithoutOverlay = import inputs.nixpkgs {inherit system;};

      # C library for accessing the PostgreSQL parser outside of the
      # server environment
      pg_query = pkgsWithoutOverlay.stdenv.mkDerivation {
        name = "pg_query";
        src = pkgsWithoutOverlay.fetchFromGitHub {
          owner = "pganalyze";
          repo = "libpg_query";
          rev = "1ec38940e5c6f09a4c1d17a46d839a881c4f2db7";
          sha256 = "X48wjKdgkAc4wUubQ5ip1zZYiCKzQJyQTgGvO/pOY3I=";
        };
        buildInputs = with pkgsWithoutOverlay; [clang which protobufc];
        patchPhase = with pkgsWithoutOverlay;
          ''
            ${gnused}/bin/sed -i.bak 's/prefix = .*/prefix = $(out)/g' Makefile
          ''
          + pkgsWithoutOverlay.lib.optionalString pkgsWithoutOverlay.stdenv.isDarwin ''
            ${gnused}/bin/sed -i.bak 's/-Wl,-soname,$(SONAME)/-Wl/g' Makefile
          '';
      };

      overlay = final: prev: {
        haskell =
          prev.haskell
          // {
            packageOverrides = hfinal: hprev:
              prev.haskell.packageOverrides hfinal hprev
              // {
                pg-query = hfinal.callCabal2nix "pg-query" inputs.hs_pg-query {inherit pg_query;};

                hasql-generator =
                  final.haskell.lib.appendConfigureFlags
                  (hfinal.callCabal2nix "hasql-generator" ./. {inherit pg_query;})
                  [
                    "--extra-include-dirs=${pg_query}/include"
                    "--extra-lib-dirs=${pg_query}/lib"
                  ];
              };
          };
        hasql-generator = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.hasql-generator;
      };

      pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [overlay];
      };
      unstablePkgs = import inputs.nixpkgs-unstable {
        inherit system;
        overlays = [overlay];
      };

      hspkgs = unstablePkgs.haskellPackages;

      tmp-postgres = inputs.tmp-postgres.packages;
    in {
      devShell = pkgs.haskell.packages.ghc98.shellFor {
        withHoogle = true;
        packages = p: [p.hasql-generator];
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

          ## HLS
          (pkgs.haskell-language-server.override {supportedGhcVersions = ["982"];})

          ## HLint
          hspkgs.hlint

          ## hpack (to avoid needing to generate .cabal files)
          hspkgs.hpack

          # Dev UX

          ## https://pre-commit.com/
          pkgs.pre-commit
        ];
      };

      packages.default = pkgs.haskellPackages.hasql-generator;
    });
}
