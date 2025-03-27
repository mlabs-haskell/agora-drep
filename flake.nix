{
  description = "uplc-benchmark";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    pre-commit-hooks-nix = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hci-effects = {
      url = "github:hercules-ci/hercules-ci-effects";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-parts.follows = "flake-parts";
    };
    simpleHaskellNix = {
      url = "github:mlabs-haskell/simple-haskell-nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        pre-commit-hooks-nix.follows = "pre-commit-hooks-nix";
        hci-effects.follows = "hci-effects";
      };
    };
    plutarch = {
      url = "github:Plutonomicon/plutarch-plutus?ref=7b346d00596531d3682204e226f7457d51849a21";
      flake = false;
    };
    plutus-test = {
      url = "github:mlabs-haskell/plutus-test/szg251/fix";
      flake = false;
    };
    cardano-haskell-packages = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };
  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } (
      { self, ... }:
      {
        imports = [
          inputs.pre-commit-hooks-nix.flakeModule
          inputs.hci-effects.flakeModule
          inputs.simpleHaskellNix.flakeModules.simpleHaskellNix

          ./nix/latex
          ./nix/mdbook
          ./nix/agora-drep

          ./specification
          ./website
        ];

        systems = inputs.nixpkgs.lib.systems.flakeExposed;

        herculesCI.ciSystems = [ "x86_64-linux" ];

        hercules-ci.flake-update = {
          enable = true;
          updateBranch = "hci/update-flake-lock";
          createPullRequest = true;
          autoMergeMethod = null;
          when = {
            minute = 45;
            hour = 12;
            dayOfWeek = "Sun";
          };
        };

        perSystem =
          {
            config,
            pkgs,
            lib,
            system,
            ...
          }:
          {
            _module.args.pkgs = import self.inputs.nixpkgs {
              inherit system;
              config.allowBroken = true;
            };

            pre-commit.settings = {
              hooks = {
                fourmolu.enable = true;
                cabal-fmt.enable = true;
                latexindent.enable = true;
                nixfmt-rfc-style.enable = true;
              };

              settings = {
                latexindent.flags = lib.concatStringsSep " " [
                  "--yaml=\"defaultIndent:'  ', onlyOneBackUp: 1\""
                  "--local"
                  "--silent"
                  "--overwriteIfDifferent"
                  "--logfile=/dev/null"
                ];
                typos.ignored-words = [ "wheres" ];
              };
            };

            devShells = {
              default = pkgs.mkShell {
                shellHook = config.pre-commit.installationScript;
                inputsFrom = [ config.devShells.agora-drep ];

                nativeBuildInputs = [
                  pkgs.fd
                  pkgs.texlive.combined.scheme-full
                ];
              };
            };
          };
      }
    );
}
