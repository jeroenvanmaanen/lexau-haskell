#!/bin/bash

set -e -x

BIN="$(dirname "$0")"
BIN="$(cd "$BIN" ; pwd)"

if [ ".$1" = ".--project" ]
then
	PROJECT="$2"
	shift 2
else
	exec "$BIN/update-and-run.sh" "$(basename "$0")" "$@"
fi

echo '>>> Start clobber' "$@" >&2

svn revert -R "$PROJECT"
svn status -u "$PROJECT" | sed -n 's/^[?] *[*] *//p' | tr '\012' '\000' | xargs -0 rm -rf
svn status -u "$PROJECT"

if [ ".$1" = '.--update' ]
then
	shift
	svn update "$PROJECT" "$@"
fi

##EOF
