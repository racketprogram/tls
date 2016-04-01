First, check that the universe repository is enabled by inspecting '/etc/apt/sources.list' with your favourite editor.
You will need to use sudo to ensure that you have permissions to edit the file.

    sudo nano /etc/apt/sources.list

If universe is not included then modify the file so that it does.

    deb http://us.archive.ubuntu.com/ubuntu vivid main universe

After any changes you should run this command to update your system.

    sudo apt-get update
    sudo apt-get install mit-scheme