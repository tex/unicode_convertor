#!/bin/bash

wget -c -nv --no-parent --mirror ftp://unicode.org/Public/MAPPINGS/

erlc "generate.erl"

M="unicode_convertor"

TMP="mappings.tmp"
rm -f "$TMP"

(
echo "% See MIT-LICENSE for licensing information."
echo
echo "-module('$M')."
echo
echo "% Automatically generated file by unicode_convertor/gen.sh"
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
    ./unicode.org/Public/MAPPINGS/ISO8859/8859-*.TXT \
    ./unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/*.TXT
do
    (
    BASE=$(basename $FILE)
    echo
    echo "#NAME:"${BASE%.*}
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
