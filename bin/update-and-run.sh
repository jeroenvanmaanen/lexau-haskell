#!/bin/bash

set -e -x

echo '>>> Start update and run' "$@" >&2

BIN="$(dirname "$0")"
BIN="$(cd "$BIN" ; pwd)"
PROJECT="$(dirname "$BIN")"

SCRIPT_NAME="$1"
shift
SCRIPT_PATH="$BIN/$SCRIPT_NAME"

[ ".$1" != ".--" ] || shift

svn update "$SCRIPT_PATH"
STATUS="$(svn status "$SCRIPT_PATH" | cut -c1)"
echo "STATUS=[$STATUS]"
if [ ".$STATUS" = 'C' ]
then	svn merge --accept theirs-full "$SCRIPT_PATH"
fi

if [ -z "$TMP" ]
then
	TMP=/tmp
fi

SCRIPT_COPY="$TMP/$SCRIPT_NAME"
cp "$SCRIPT_PATH" "$SCRIPT_COPY"
exec "$SCRIPT_COPY" --project "$PROJECT" "$@"
