#!/bin/sh

# Install missing library:
sudo apt-get update
sudo apt-get install -y libnode64

R -e "renv::install('rstan')"