{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
  };

  outputs =
    {
      self,
      nixpkgs,
    }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

      haskellFixes =
        let
          overrideCabal =
            f: drv:
            (drv.override (
              args:
              args
              // {
                mkDerivation = drv: (args.mkDerivation drv).override f;
              }
            ))
            // {
              overrideScope = scope: overrideCabal f (drv.overrideScope scope);
            };
          doJailbreak = overrideCabal (drv: {
            jailbreak = true;
          });
          dontCheck = overrideCabal (drv: {
            doCheck = false;
          });
          unmarkBroken = overrideCabal (drv: {
            broken = false;
          });
        in
        pkgs: final: prev: {
          # rel8 = dontCheck prev.rel8;
          # hs-opentelemetry-propagator-datadog = unmarkBroken prev.hs-opentelemetry-propagator-datadog;
          # migrant-postgresql-simple = dontCheck (
          #   doJailbreak (final.callHackage "migrant-postgresql-simple" "0.1.0.3" { })
          # );
          # annotated-exception = final.callHackageDirect {
          #   pkg = "annotated-exception";
          #   ver = "0.3.0.4";
          #   sha256 = "sha256-ite2CoTZe3tLayZyvaHpyRmL4hcLILUxCKj8YIbEGD4=";
          # } { };
          # hasql = dontCheck (final.callHackage "hasql" "1.8.1.4" { });
          # hasql-pool = dontCheck (final.callHackage "hasql-pool" "1.2.0.3" { });
          # rel8 = dontCheck prev.rel8;
          # hasql-transaction = dontCheck (final.callHackage "hasql-transaction" "1.1.1.2" { });
          # postgresql-binary = dontCheck (final.callHackage "postgresql-binary" "0.14" { });
          # postgresql-libpq = dontCheck (final.callHackage "postgresql-libpq" "0.11.0.0" { });
          # postgresql-libpq-configure = overrideCabal (drv: {
          #   broken = false;
          #   libraryPkgconfigDepends = [ pkgs.postgresql ];
          # }) prev.postgresql-libpq-configure;
        };

      localPackagesOverlay = final: prev: {
        hoogle-mcp = final.callCabal2nix "hoogle-mcp" ./. { };
      };

      haskellOverlay =
        pkgs:
        nixpkgs.lib.composeManyExtensions [
          (haskellFixes pkgs)
          localPackagesOverlay
        ];

      haskellPackages = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        pkgs.haskellPackages.extend (haskellOverlay pkgs)
      );
    in
    {
      overlays = {
        default = final: prev: {
          hoogle-mcp =
            final.haskell.lib.justStaticExecutables
              self.haskellPackages.${final.system}.hoogle-mcp;
        };
        inherit haskellOverlay;
      };

      inherit haskellPackages;
      legacyPackages = forAllSystems (
        system:
        import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
        }
      );

      packages = forAllSystems (system: {
        default = self.packages.${system}.hoogle-mcp;

        inherit (self.legacyPackages.${system}) hoogle-mcp;
      });

      devShells = forAllSystems (system: {
        default = self.haskellPackages.${system}.shellFor {
          withHoogle = true;
          packages = p: [
            (nixpkgs.legacyPackages.${system}.haskell.lib.doHaddock p.hoogle-mcp)
          ];
          buildInputs =
            (
              let
                pkgs = self.legacyPackages.${system};
              in
              [
                pkgs.ghcid
                pkgs.cabal-install

                pkgs.mprocs
                pkgs.watchexec
                pkgs.just

                pkgs.tailwindcss_4
                pkgs.watchman

                pkgs.sops
                pkgs.google-cloud-sdk

                # Database
                (pkgs.postgresql.withPackages (ps: [ ps.pg_uuidv7 ]))
                (pkgs.writeShellScriptBin "db" ''
                  if [ ! -d $PGHOST ]; then
                    mkdir -p $PGHOST
                  fi
                  if [ ! -d $PGDATA ]; then
                    echo 'Initializing postgresql database...'
                    initdb $PGDATA --auth=trust >/dev/null
                  fi
                  exec postgres \
                    -c listen_addresses= \
                    -c unix_socket_directories=$PGHOST \
                    -c log_statement=all \
                    -c logging_collector=off \
                    -c log_destination=stderr \
                    -c log_min_messages=notice
                '')
              ]
            )
            ++ (with self.haskellPackages.${system}; [
              cabal-gild
              cabal-add
              haskell-language-server
              hlint
            ]);
        };
      });

      checks = forAllSystems (system: {
        inherit (self.packages.${system})
          default
          ;
      });
    };
}
