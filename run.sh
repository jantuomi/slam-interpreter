#!/bin/bash

set -uo pipefail
ghc -v0 -o interpreter src/*.hs
./interpreter $@
