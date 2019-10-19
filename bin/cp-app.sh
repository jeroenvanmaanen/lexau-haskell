#!/bin/bash

BIN="$(dirname "$0")"
BIN="$(cd "$BIN" ; pwd)"

. "$BIN/settings.sh"

SSH_DEST="$SSH_DESTINATION"
if [ -n "$1" -a ".$1" != '.--' ]
then
	SSH_DEST="$1"
	shift
fi

echo "$(date): [$SSH_GATEWAY]: [$SSH_DEST]"

PROJECT="$(dirname "$BIN")"
COMMAND='tar -C "$HOME/src/lexau" -xvzf -'
(
	cd "$PROJECT" || exit 1
	svn status \
		| sed -n -e 's/^[AM] *//p' \
		| tar -cf - -T - --exclude '**/.svn' \
		| gzip -c \
		| ssh -e none "$SSH_GATEWAY" "ssh -e none '$SSH_DEST' '$COMMAND'"
)

if [ "$#" -ge 1 ]
then
	ssh -e none "$SSH_GATEWAY" "ssh -e none '$SSH_DEST' \"\$HOME/src/lexau/bin/build.sh\" $@" < /dev/null
fi
