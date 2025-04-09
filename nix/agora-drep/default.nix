{ inputs, ... }:
{
  perSystem =
    {
      simpleHaskellNix,
      pkgs,
      self',
      ...
    }:
    let
      agora-drep = simpleHaskellNix.mkPackage {
        name = "agora-drep";
        src = ./../../.;

        externalRepositories = {
          "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.cardano-haskell-packages;
        };

        externalDependencies = [
          "${inputs.plutarch}"
          "${inputs.plutarch}/plutarch-ledger-api"
          "${inputs.plutarch}/plutarch-testlib"
          "${inputs.plutarch}/plutarch-orphanage"
          "${inputs.plutus-test}/plutus-context-builder"
          "${inputs.plutus-test}/plutus-unit"
        ];
      };
    in
    {
      inherit (agora-drep) checks apps;

      devShells.agora-drep = agora-drep.devShell;

      packages = agora-drep.packages // {
        compiled-scripts =
          pkgs.runCommand "compiled-scripts"
            {
              nativeBuildInputs = [
                self'.packages."agora-drep:exe:agora-drep-script-export"
              ];
            }
            ''
              agora-drep-script-export
              mv scripts $out
            '';
      };
    };
}
