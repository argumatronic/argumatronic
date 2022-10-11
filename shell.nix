let
  nixpkgs = import (builtins.fetchTarball {

    # Descriptive name to make the store path easier to identify
    name = "nixos-20.03-2021-05-03";

    url = "https://github.com/nixos/nixpkgs/archive/1db42b7fe3878f3f5f7a4f2dc210772fd080e205.tar.gz";

    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "05k9y9ki6jhaqdhycnidnk5zrdzsdammbk5lsmsbz249hjhhgcgh";

  }) {};

  ghc = nixpkgs.haskell.packages.ghc865.ghcWithPackages (p: [
    p.hakyll
  ]);

in
  nixpkgs.mkShell {
    LC_ALL = "en_US.UTF8";
    LOCALE_ARCHIVE = "${nixpkgs.glibcLocales}/lib/locale/locale-archive";

    nativeBuildInputs = [
      ghc
      nixpkgs.openssh
      nixpkgs.rsync
    ];
  }
