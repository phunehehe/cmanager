Install CGroup tools (reboot required)

    sudo apt-get install cgroup-bin

Dependencies

    cabal install happstack-lite filemanip http

sudo cgcreate -t vagrant -g cpu,memory:awesome_group

cgexec -g cpu,memory:awesome_group bash
