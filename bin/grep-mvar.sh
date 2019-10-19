#!/bin/bash

set -e

BIN="$(dirname "$0")"
BIN="$(cd "$BIN" ; pwd)"
PROJECT="$(dirname "$BIN")"

IMPORT="$(grep '^import Control[.]Concurrent[.]MVar' "$PROJECT/src/LExAu/Pipeline/Concurrent.hs")"

function make-pattern() {
	local IMPORT="$1"
	echo "$IMPORT" \
		| sed -E \
			-e 's/^[^(]*[(]/|/' \
			-e 's/[)][^)]*$/|/' \
			-e 's/, */|/g' \
			-e 's/[|](new(Empty)?)?MVar[|]/|/g' \
			-e 's/[|](new(Empty)?)?MVar[|]/|/g' \
			-e 's/^[|]//' \
			-e 's/[|]$//'
}

PATTERN="$(make-pattern "$IMPORT")"

cd "$PROJECT"
sfind.sh src -type f -print0 | xargs -0 egrep -n "$PATTERN" /dev/null
