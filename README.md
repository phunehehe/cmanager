# API


## Get All Groups

  - URL: `/groups`
  - Method: GET


## Get Tasks in a Group

  - URL: `/groups/<group_name>`
  - Method: GET

`<group_name>` should be URL-encoded


## Add a Task to a Group

  - URL: `/groups/<group_name>`
  - Method: POST

`<group_name>` should be URL-encoded

Parameters:

  - `pid`: The PID of the task to be added to the group


# Installation

 1. Recommended: put the source code in `/opt/cmanager`
 2. Include `module.nix` from `/etc/nixos/configuration.nix`
 3. If the source code is not in `/opt/cmanager`, override `cmanager.siteDir`
    appropriately
 4. Run `nixos-rebuild switch`
