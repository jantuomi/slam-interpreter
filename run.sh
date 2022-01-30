#!/bin/bash

set -uo pipefail
ghc -o interpreter src/*.hs
./interpreter $@
ret=$?
rm src/*.{hi,o}
exit $ret
