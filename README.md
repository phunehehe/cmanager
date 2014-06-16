# CManager

CManager is a web service that manages CGroups. It provides a website for human
use, as well as an API for machine use.


## Website

Just visit `localhost`, intuition will lead your way from there.


## API Specs


### Get All Groups

  - URL: `/api/groups`
  - Method: GET


### Get Tasks in a Group

  - URL: `/api/groups/<group_name>`
  - Method: GET

`<group_name>` should be URL-encoded


### Add a Task to a Group

  - URL: `/api/groups/<group_name>`
  - Method: POST

`<group_name>` should be URL-encoded

Parameters:

  - `pid`: The PID of the task to be added to the group


## Installation

 1. Recommended: put the source code in `/opt/cmanager`
 2. Include `module.nix` from `/etc/nixos/configuration.nix`
 3. If the source code is not in `/opt/cmanager`, override `cmanager.siteDir`
    appropriately
 4. Run `nixos-rebuild switch`
