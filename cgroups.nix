{ config, lib, pkgs, ... }:

let
    siteDir = config.cgroupsManager.siteDir;
    callPackage = package:
        pkgs.lib.callPackageWith pkgs.haskellPackages package { config = config; };
    cgroupsManager = callPackage "${siteDir}/package.nix";

in {

    options = {
        cgroupsManager.siteDir = lib.mkOption {
            type = lib.types.str;
            # TODO: detect this automatically
            default = "/cgroups";
        };
    };

    config = {

        nixpkgs.config.allowUnfree = true;
        environment.systemPackages = [ cgroupsManager ];

        services.nginx.enable = true;
        services.nginx.httpConfig = ''
            types {
                text/html                html;
                text/css                 css;
                application/x-javascript js;
            }
            server {
                root ${siteDir}/static;
                location / {
                    try_files $uri @happstack;
                }
                location @happstack {
                    proxy_pass http://localhost:8000;
                }
            }
        '';

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
    };
}
