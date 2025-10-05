#! /bin/bash

SCRIPT_DIR=$(dirname "$(readlink -f "$0")")

git show HEAD:$1 | node $SCRIPT_DIR/cli-disassemble.js > /tmp/old.txt
cat $1 | node $SCRIPT_DIR/cli-disassemble.js > /tmp/new.txt
diff -u /tmp/old.txt /tmp/new.txt
