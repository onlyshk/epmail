#!/bin/bash

if [ -z "$TEMP" ]; then
 TEMP='/tmp';
fi;

$@ > "$TEMP/$$.stdout.log" 2> "$TEMP/$$.stderr.log" &

echo "fork: \"$@\" : $$"
