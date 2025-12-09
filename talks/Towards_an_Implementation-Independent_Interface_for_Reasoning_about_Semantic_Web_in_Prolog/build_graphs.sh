#!/bin/sh

set -xe

pl="scryer-prolog --no-add-history -f"

function build() {
  $pl -g 'gengraph("'"${2}"'", '"${1}"')' -g halt graph.pl
  exec dot -Tsvg -O "${2}"
}

build wiki "prolog-graph.dot" &
build blank "blank-graph.dot" &
build svo "svo.dot" &
build open_world "open-world.dot" &

wait $(jobs -rp)
