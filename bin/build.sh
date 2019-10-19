#!/bin/bash

set -e -x

DURATION=5

BIN="$(dirname "$0")"
BIN="$(cd "$BIN" ; pwd)"
PROJECT="$(dirname "$BIN")"

cd "$PROJECT" || exit 1
pwd

echo ">>> Start build: $@" >&2

if [ ".$1" = '.--' ]
then
	shift
fi

if [ ".$1" = '.--clean' ]
then
	shift
	make clean
fi

make

if [ ".$1" = '.--killall' ]
then
	shift
	killall lexau || true
	ps | grep -i lexau || true
fi

if [ ".$1" = '.--run' ]
then
	shift
	nohup bin/test-single.sh < /dev/null &
	PID="$!"
	sleep 1
	netstat -lntp
	curl -sS -D - -F action=stop http://localhost:8000/recorder/stop/now || true
	echo
fi

if [ ".$1" = '.--kill' ]
then
	shift
	if [ -n "$PID" ]
	then
		sleep "$DURATION"
		kill "$PID" || true
		ps | grep -i lexau || true
		ps --pid "$PID"
	fi
fi

if [ ".$1" = '.--killall' ]
then
	shift
	sleep "$DURATION"
	killall lexau || true
	ps | grep -i lexau || true
fi

if [ ".$1" = '.--show-log' ]
then
	shift
	LOG_FILE='dist/build/lexau/log/lexau-single.log'
	ls -ld "$LOG_FILE" >&2
	cat "$LOG_FILE" >&2
fi

##EOF
