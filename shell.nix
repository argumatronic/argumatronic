{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

pkgs.haskell.lib.buildStackProject {
  name = "argumatronic.com";
  inherit ghc;
  buildInputs = with pkgs; [ zlib rsync openssh ];
  LANG = "en_US.UTF-8";
  TMPDIR = "/tmp";
}