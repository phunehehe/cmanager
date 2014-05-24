Install CGroup tools (reboot required)

    sudo apt-get install cgroup-bin

Dependencies

    cabal install happstack-lite filemanip http split

sudo cgcreate -a vagrant -g cpu,memory:awesome_group

cgexec -g cpu,memory:awesome_group bash
