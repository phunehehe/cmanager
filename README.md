# Installation

 1. Recommended: put the source code in `/opt/cmanager`
 2. Include `cmanager.nix` from `/etc/nixos/configuration.nix`
 3. If the source code is not in `/opt/cmanager`, override `cmanager.siteDir`
    appropriately
 4. Run `nixos-rebuild switch`
