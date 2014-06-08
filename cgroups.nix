{ lib, pkgs, ... }:

let
    cgroupsManager = pkgs.lib.callPackageWith pkgs.haskellPackages /cgroups/package.nix {};

in {

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
            root /cgroups/static;
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
}
