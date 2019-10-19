#!/bin/bash

set -e

SED_EXT=-r
case $(uname) in
Darwin*)
        SED_EXT=-E
esac

SVNREVISION="$(svnversion)"
echo "SVNREVISION: [$SVNREVISION]"
CURRENT="$(echo "$SVNREVISION" | sed -e 's/^[^:]*://' -e 's/[A-Za-z]*$//')"
if [ -z "$CURRENT" ]
then
	echo "Not in subversion" >&2
	exit 0
fi

echo "CURRENT: [$CURRENT]"

LAST=
if [ \! -f svndates.csv ]
then
	touch svndates.csv
fi
LAST="$(sed -e 's/:.*$//' -e 1q svndates.csv)"
if [ -z "$LAST" ]
then
	LAST=1
fi

if [ "$LAST" -lt "$CURRENT" ]
then
	mv svndates.csv svndates.csv~
	svn log -r "$CURRENT:$LAST" | sed -n "$SED_EXT" -e 's/^r([0-9]*) [|] [^|]* [|] ([-0-9]*) .*/\1: \2/p' > svndates.csv
	tail +2 svndates.csv~ >> svndates.csv
fi

NEW_LAST="$(sed -e 's/:.*$//' -e 1q svndates.csv)"
if [ "$NEW_LAST" -lt "$CURRENT" ]
then
	mv svndates.csv svndates.csv~
	echo "$CURRENT: *" > svndates.csv
	cat svndates.csv~ >> svndates.csv
fi

COMMITTED="$(awk -F ": " "/:.*[^-0-9 ]/ { next; } \$1 > $CURRENT { next; } { print; exit }" svndates.csv)"
echo "COMMITTED: [$COMMITTED]"
DATE="${COMMITTED#*: }"
echo "COMMITTED DATE: [$DATE]"
COMMITTED="${COMMITTED%: *}"
echo "COMMITTED: [$COMMITTED]"

CLEAN_REV="$(echo "$SVNREVISION" | grep -v '[-:M]' || true)"
echo "CLEAN REVISION: [$CLEAN_REV]"

if [ -z "$CLEAN_REV" ]
then
	COMMITTED="$COMMITTED*"
fi

cat <<EOT > svnrevision.tex
\def\SvnRevision/{$COMMITTED}
\def\SvnDate/{$DATE}
EOT
pdflatex rapport
echo "COMMITTED: [$COMMITTED]"
echo "COMMITTED DATE: [$DATE]"
date

if [ -n "$CLEAN_REV" ]
then
    NAME="$(pwd)"
    NAME="$(basename "$NAME")-r$SVNREVISION"
    cp 'rapport.pdf' "$NAME.pdf"
    echo "NAME: $NAME.pdf"
fi
