#!/bin/sh

set -xe

FILE="main.pl"
DIR="$PWD"

pushd ..
scryer-prolog --no-add-history -f -g main -g halt "${DIR}/${FILE}"
popd
