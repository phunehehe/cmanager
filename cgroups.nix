{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [

        binutils
        gcc
        zlib

        haskellPackages.cabal2nix
        haskellPackages.cabalInstall
        haskellPackages.ghc

        haskellPackages.happstackLite
        haskellPackages.filemanip
        haskellPackages.split
        haskellPackages.HTTP
        haskellPackages.text
    ];
}
