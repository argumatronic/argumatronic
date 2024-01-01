{
  inputs = {
    "nixos-23.11".url = "github:NixOS/nixpkgs/nixos-23.11";
    "nixos-unstable".url = "github:NixOS/nixpkgs/nixos-unstable";
    "flake-utils".url = "github:numtide/flake-utils";
  };
  outputs = inputs@{ self, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        nixpkgs = {
          "nixos-23.11" = import inputs."nixos-23.11" { inherit system; };
          "nixos-unstable" = import inputs."nixos-unstable" { inherit system; };
        };
        pkgs = nixpkgs."nixos-23.11";
        inherit (pkgs) runCommand crane;
        inherit (pkgs.lib) fold composeExtensions concatMap attrValues;
        inherit (pkgs.haskell.lib) justStaticExecutables;

        combineOverrides = old:
          fold composeExtensions (old.overrides or (_: _: { }));

        haskellPackages = pkgs.haskellPackages.override {
          overrides = new: old: {
            argumatronic = new.callPackage ./argumatronic.nix {};
          };
        };

        argumatronic = justStaticExecutables haskellPackages.argumatronic;

        htmlPages = runCommand
          "argumatronic-content"
          {
            buildInputs = [ argumatronic ];
            env.LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
            env.LANG = "en_US.UTF-8";
          }
          ''
            mkdir -p .
            ln -s "${./content}" content
            ls content/
            site build
            mv _site "$out"
          '';

        container = pkgs.dockerTools.buildLayeredImage {
          name = "argumatronic";
          contents = [ pkgs.busybox htmlPages ];
          config = {
            Cmd = [ "${pkgs.busybox}/bin/busybox" "httpd" "-f" "-h" "${htmlPages}" ];
            ExposedPorts."80/tcp" = { };
          };
        };

        deploy = pkgs.writeShellApplication {
          name = "deploy-argumatronic";
          runtimeInputs = [ crane ];
          text = ''
            cleanup() {
              rm -rf "$tmp"
            }
            trap cleanup EXIT
            tmp="$(mktemp -d)"
            nix build .#container --out-link "$tmp/container.tar.gz"
            gunzip --force "$tmp/container.tar.gz" > "$tmp/container.tar"
            crane push "$tmp/container.tar" registry.digitalocean.com/iowa/argumatronic:latest
          '';
        };

      in {
        packages = { inherit argumatronic container htmlPages crane; };
        devShells.deploy = pkgs.mkShell { packages = [ deploy ]; };
      }
    );
}
