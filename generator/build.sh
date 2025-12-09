#!/bin/sh

set -xe

FILE="main.pl"
DIR="$PWD"

cd ..
exec scryer-prolog --no-add-history -f -g main -g halt "${DIR}/${FILE}"
