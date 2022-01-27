#!/bin/bash

set -uxo pipefail
ghc -o interpreter src/*.hs
./interpreter $@
