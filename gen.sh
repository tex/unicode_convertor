#!/bin/bash

wget -c -nv --no-parent --mirror ftp://unicode.org/Public/MAPPINGS/

erlc "generate.erl"

M="unicode_convert"

TMP="mappings.tmp"
rm -f "$TMP"

(
echo "-module('$M')."
echo
echo "% Automatically generated file by to_unicode/gen.sh"
echo "% at $(date)."
echo
echo "-export([map/2, to_capital/2, to_small/2, remove_accent/2, m/2, c/1, s/1, r/1])."
echo
echo "map(To, {From, Input}) -> lists:map(fun(I) -> m(To, {From, I}) end, Input)."
echo "to_capital(Code, Input) -> lists:map(fun(I) -> c({Code, I}) end, Input)."
echo "to_small(Code, Input) -> lists:map(fun(I) -> s({Code, I}) end, Input)."
echo "remove_accent(Code, Input) -> lists:map(fun(I) -> r({Code, I}) end, Input)."
echo
) > "$M.erl"
for FILE in \
	./unicode.org/Public/MAPPINGS/ISO8859/8859-*.TXT
do
    (
    echo
    cat "$FILE"
    echo
    ) >> "$TMP"
done
(
echo
cat "$TMP" | erl -noshell -s generate generate
echo
) >> "${M}.erl"

rm -f "$TMP"

erlc "${M}.erl"
