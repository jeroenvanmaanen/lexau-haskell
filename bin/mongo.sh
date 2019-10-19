#!/bin/bash

BIN="$(dirname "$0")"
BIN="$(cd "$BIN" ; pwd)"

LINK="$(readlink "$0")"
if [ -n "$LINK" ]
then
    LINK="$(dirname "$LINK")"
    BIN="$(cd "$BIN" ; cd "$LINK" ; pwd)"
fi

. "$BIN/settings.sh"

SCRIPT="$0"
JS="${SCRIPT%.sh}.js"
echo "JS=[$JS]"

mongo "$DBHOST/lexau" "$JS"
