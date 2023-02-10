#!/bin/sh

# Execute this script to install pandoc. Needed to render rmarkdown files
wget https://github.com/jgm/pandoc/releases/download/2.13/pandoc-2.13-1-amd64.deb
dpkg -i pandoc-2.13-1-amd64.deb
rm pandoc-2.13-1-amd64.deb