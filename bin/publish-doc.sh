#!/bin/bash

set -e

BIN="$(dirname "$0")"
BIN="$(cd "$BIN" ; pwd)"
PROJECT="$(dirname "$BIN")"

source "$BIN/settings.sh"

if [ -z "$DOC_DIR" ]
then
	echo "DOC_DIR not set" >&2
	exit 1
fi

if [ \! -d "$DOC_DIR" ]
then
	echo "Not found or not a directory: $DOC_DIR" >&2
	exit 1
fi

rm -rf "$DOC_DIR/lexau"

tar -C "$PROJECT/dist/doc/html/lexau" -cvf - lexau | tar -C "$DOC_DIR" -xvf -
