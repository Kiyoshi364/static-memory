#!/bin/sh

set -xe

FILE="main.pl"

scryer-prolog --no-add-history -f -g "main_file('../readme.md')" $FILE </dev/null
