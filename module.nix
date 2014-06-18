{ config, lib, pkgs, ... }:

let
    siteDir = config.cmanager.siteDir;
    callPackage = package:
        pkgs.lib.callPackageWith pkgs.haskellPackages package { config = config; };
    cmanager = callPackage "${siteDir}/default.nix";

in {

    options = {
        cmanager.siteDir = lib.mkOption {
            type = lib.types.str;
            # TODO: detect this automatically
            default = "/opt/cmanager";
        };
    };

    config = {

        environment.systemPackages = [ cmanager ];

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

        systemd.services.cmanager = {
            description = "CManager";
            wantedBy = [ "multi-user.target" ];
            after = [ "network.target" ];
            path = [ cmanager ];
            serviceConfig = {
                ExecStart = "${cmanager}/bin/cmanager";
                Restart = "always";
            };
        };
    };
}
