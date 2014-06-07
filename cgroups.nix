{ lib, pkgs, ... }:

let
    cgroupsManager = pkgs.lib.callPackageWith pkgs.haskellPackages /cgroups/package.nix {};

in {

    nixpkgs.config.allowUnfree = true;
    environment.systemPackages = [ cgroupsManager ];

    systemd.services.cgroups-manager = {
        description = "CGroups Manager";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        path = [ cgroupsManager ];
        serviceConfig = {
            ExecStart = "${cgroupsManager}/bin/cgroups-manager";
            Restart = "always";
        };
    };
}
