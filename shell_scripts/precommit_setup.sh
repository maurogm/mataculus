#!/bin/sh

# Execute this script to set up pre-commits in your local repository
pip3 install pre-commit
pre-commit install

R -e "install.packages('precommit', dependencies = TRUE)"
R -e "precommit::use_precommit()"