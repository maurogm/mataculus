# Metaculus
A repo to centralize various scripts and ad-hoc analysis related to [Metaculus](https://www.metaculus.com) questions.

# Working with this project
This project assumes the use of:
  - `VS Code` as IDE.
  - `Docker` for containerization.
  - `remote-containers` plugin for developing inside the container.
  - `renv` for managing packages.

## Prerequisites

### Install Docker

- [for Mac](https://docs.docker.com/docker-for-mac/install/)
- [for Linux](https://docs.docker.com/install/linux/docker-ce/ubuntu/)
- [for Windows](https://docs.docker.com/docker-for-windows/install/)

After following the instructions, remember running the following:
```
sudo groupadd docker
sudo usermod -aG docker $USER
```
You may need to logout/reboot before the changes come into effect.


### Install required extensions in VSCode:

- Docker: ms-azuretools.vscode-docker
- Remote - Containers : ms-vscode-remote.remote-containers
  - Documentation available [here](https://code.visualstudio.com/docs/remote/containers)


## Setting up `renv`

1. If you have a local cache of packages in your host machine, check in `devcontainer.json` that it is correctly mounted during the image build.
   - Currently, it is assumed that `renv`'s cache is located in the `/.local/share/renv/cache` directory under the `$HOME` folder.
2. The first time you run the container after building the image, open an `R` session and run `renv::restore()` to install the packages detailed in renv.lock.
(Obs: since this is run in a container, there's no need to locally activate renv).
3. To take a snapshot containing only the dependencies listed in the `DESCRIPTION`
use `renv::snapshot(type="explicit")`.


## Additional configurations

### Optional: Enable pre-commits

To enable pre-commits, run the shell script `setup_precommit.sh`.

### Optional: Install pandoc

If you plan to render rmarkdown files, `pandoc` must be externally installed. Just run the script `install_pandoc.sh` provided inside the `shell_scripts` folder.

### Optional: Install rstan

If you plan to use `Stan` or the `rethinking` R package, you must intstall `rstan`. Just run the script `rstan_instal.sh` provided inside the `shell_scripts` folder.
