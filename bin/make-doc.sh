#!/bin/bash

SED_EXT=-r
case $(uname) in
Darwin*)
        SED_EXT=-E
esac
export SED_EXT

make-doc () {
	( make doc 2>&1 ; echo "Status: $?" ) | \
		sed -e '/:/s/^/$/' | \
		tr '$\012' '\012$' | \
		sed "$SED_EXT" \
			-e 's/GHC[.][^ $]* ?//g' \
			-e 's/[^ $]*Impl[^ $]* ?//g' \
			-e 's/[ $]*([$] *)/\1/g' \
			-e '/:[$] *$/d' \
			| \
		tr '$\012' '\012$' | \
		tr -d '$'
}

REPORT="$(make-doc)"
echo "$REPORT"
STATUS="$(echo "$REPORT" | sed -n '$s/^Status: //p')"
echo "Make returned: [$STATUS]"
exit "$STATUS"
