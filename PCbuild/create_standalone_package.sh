#!/bin/bash

export PATH="/usr/bin:${PATH}"
set -ex
set +H

cd "$(dirname "$(realpath "$0")")"
rm -rv standalone_dist || :
mkdir -p standalone_dist

# zip libs
TCLTK_LIB_DIR="$(echo ../externals/tcltk-*/amd64/lib)"
7z a -tzip standalone_dist/zip_libs.zip \
    -xr!test -xr!tests -xr!idle_test -xr!demos -xr!turtledemo -xr!ensurepip -xr!macholib \
    -xr!__pycache__ -xr!'*.pyc' \
    '../Lib/*' "${TCLTK_LIB_DIR}/tcl8.6" "${TCLTK_LIB_DIR}/tk8.6"
