#!/bin/bash

set -e

BIN="$(dirname "$0")"
BIN="$(cd "$BIN" ; pwd)"
PROJECT="$(dirname "$BIN")"

LOG_DIR="$PROJECT/dist/build/lexau/log"
ls -l "$BIN/settings.sh" || true
if [ -f "$BIN/settings.sh" ]
then
	. "$BIN/settings.sh"
fi

mkdir -p "$LOG_DIR"

declare -a OPTS
N=0

if [ -n "$SMP" ]
then
	OPTS[$N]='+RTS'
	N=$[$N+1]
	OPTS[$N]="-N$SMP"
	N=$[$N+1]
	OPTS[$N]='-RTS'
	N=$[$N+1]
fi

DBURL=
if [ -n "$DBNAME" ]
then
    DBURL="localhost/$DBNAME"
    if [ -n "$DBHOST" ]
    then
        DBURL="$DBHOST/$DBNAME"
    fi
    OPTS[$N]="-b$DBURL"
    N=$[$N+1]
fi
echo "DBURL=[$DBURL]"

echo "CORPUS=[$CORPUS]"
if [ -n "$CORPUS" ]
then
	CORPUS="$(echo "$CORPUS" | sed -e 's:///*:/:g')"
	OPTS[$N]="-i$CORPUS"
	N=$[$N+1]
fi

set -x

LOG_FILE="$LOG_DIR/lexau-single.log"
echo "LOG_FILE=[$LOG_FILE]" >&2
date > "$LOG_FILE"
echo "OPTIONS=[${OPTS[@]}]" >> "$LOG_FILE"
cat "$LOG_FILE"
echo '' >> "$LOG_FILE"

time "$PROJECT/dist/build/lexau/lexau" "${OPTS[@]}" -s -d"$LOG_DIR" "$@" 2>&1 >> "$LOG_FILE"

## In ghci: ReadWordsFile.test "/home/jeroen/corpus/kjv10-cleaner.txt" (Just "/home/jeroen/src/lexau/lexau-oracle.data") (Just "/home/jeroen/src/lexau/dist/build/lexau/log")
