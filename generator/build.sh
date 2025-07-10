#!/bin/sh

set -xe

FILE="main.pl"
README="'readme.md'"
TURTLE="'static-memory.ttl'"
DIR="$PWD"

pushd ..
scryer-prolog --no-add-history -f -g "main_md_ttl(${README}, ${TURTLE})" -g halt "${DIR}/${FILE}"
popd
