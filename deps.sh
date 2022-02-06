#!/bin/sh

set -euo pipefail

cabal install --lib hashable
cabal install --lib mtl
