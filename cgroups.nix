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


    systemd.services.cgroups-manager =
        let site_dir = "/cgroups";
        in {
        description = "CGroups Manager";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        serviceConfig = {
            ExecStart = "${site_dir}/dist/build/cgroups-manager/cgroups-manager";
            Restart = "always";
        };
    };
}
