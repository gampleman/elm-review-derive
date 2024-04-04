#!/bin/bash

set -e

SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"
cat << EOF > $SCRIPT_DIR/../tests/StandardModule.elm
module StandardModule exposing (standard)

-- Do not edit this module directly, it is generated from ../e2e/Standard.elm


standard : String
standard =
    """$(cat $SCRIPT_DIR/Standard.elm)"""
EOF


cp $SCRIPT_DIR/E2E.elm $SCRIPT_DIR/../tests/E2E.elm 
cp $SCRIPT_DIR/Standard.elm $SCRIPT_DIR/../tests/Standard.elm 
npx elm-review --config ./preview --fix-all-without-prompt $SCRIPT_DIR/../tests

npx elm-test $SCRIPT_DIR/../tests/E2E.elm

rm -f $SCRIPT_DIR/../tests/E2E.elm  $SCRIPT_DIR/../tests/Standard.elm 
