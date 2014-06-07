{ config, pkgs, ... }:

{
    environment.systemPackages = with pkgs; [

        haskellPackages.cabalInstall
        haskellPackages.ghc

        haskellPackages.happstackLite
        haskellPackages.filemanip
        haskellPackages.split
        haskellPackages.HTTP
        haskellPackages.text
    ];
}
