#!/bin/sh

cd "$(dirname "$0")"
set -e

elm-package install -y

#ELM_CONSOLE_VERSION_DIR="$(ls elm-stuff/packages/laszlopandy/elm-console/)"
#ELM_IO_PATH="elm-stuff/packages/laszlopandy/elm-console/$ELM_CONSOLE_VERSION_DIR/elm-io.sh"

#elm-make --yes --output game.html game.elm
##bash $ELM_IO_PATH raw-test.js test.js
