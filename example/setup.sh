#!/bin/sh

cd "$(dirname "$0")"
set -e

echo "before"
find .

elm-package install -y

echo "after"
find .

#ELM_CONSOLE_VERSION_DIR="$(ls elm-stuff/packages/laszlopandy/elm-console/)"
#ELM_IO_PATH="elm-stuff/packages/laszlopandy/elm-console/$ELM_CONSOLE_VERSION_DIR/elm-io.sh"

elm-make --yes --output example.html example.elm
##bash $ELM_IO_PATH raw-test.js test.js

echo "after compile"
ls
