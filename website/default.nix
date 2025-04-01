{
  hercules-ci.github-pages.branch = "master";
  perSystem =
    { self', config, ... }:
    {
      hercules-ci.github-pages.settings.contents = config.packages.website;

      mdBook = {
        website = {
          src = ./.;
          preBuild = ''
            mkdir -p static
            cp ${self'.packages.specification}/specification.pdf static/agora-drep-specification.pdf
            cp -r ${
              self'.packages."agora-drep:lib:agora-drep".passthru.haddock.doc
            }/share/doc/agora-drep/html ./haddock
          '';
        };
      };
    };
}
