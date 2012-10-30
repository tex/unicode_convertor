#!/bin/bash

#wget -c -nv --no-parent --mirror ftp://unicode.org/Public/MAPPINGS/

DIR="gen"
rm -rf $DIR
mkdir $DIR
pushd $DIR

erlc "../src/generate.erl"
erlc "../src/generate_map.erl"
erlc "../src/generate_capital_small_remove_accent.erl"

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
echo "-export([map/2, to_upper/2, to_lower/2, remove_accent/2, c/1, s/1, r/1])."
echo
echo "map(unicode, {From, Input}) -> lists:map(fun(I) -> apply(From, m, [unicode, {From, I}]) end, Input);"
echo "map(To, {unicode, Input}) -> lists:map(fun(I) -> apply(To, m, [To, {unicode, I}]) end, Input)."
echo "to_upper(Code, Input) -> lists:map(fun(I) -> c({Code, I}) end, Input)."
echo "to_lower(Code, Input) -> lists:map(fun(I) -> s({Code, I}) end, Input)."
echo "remove_accent(Code, Input) -> lists:map(fun(I) -> r({Code, I}) end, Input)."
echo
) > "$M.erl"
for FILE in \
    ../unicode.org/Public/MAPPINGS/ISO8859/8859-*.TXT \
    ../unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/*.TXT
do
    BASE=$(basename $FILE)
    NAME0=${BASE%.*}    # Strip extension of the file
    NAME1=${NAME0,,}    # Convert to lower case
    NAME2=${NAME1/"8859-"/"iso-8859-"}

    rm -rf "${TMP}_1"

    (
    echo
    echo "#NAME:"${NAME2}
    cat "$FILE"
    echo
    ) > "${TMP}_1"
    (
    echo "% See MIT-LICENSE for licensing information."
    echo
    echo "-module('${NAME2}')."
    echo
    echo "% Automatically generated file by unicode_convertor/gen.sh"
    echo "% at $(date)."
    echo
    echo "-export([m/2])."
    echo
    cat "${TMP}_1" | erl -noshell -s generate_map generate_map
    echo
    )> "${NAME2}.erl"

    rm -f "${TMP}_1"

    erlc "${NAME2}.erl"

    (
    echo
    echo "#NAME:"${NAME2}
    cat "$FILE"
    echo
    ) >> "$TMP"
done
(
echo
cat "$TMP" | erl -noshell -s generate_capital_small_remove_accent generate_capital_small_remove_accent
echo
) >> "${M}.erl"

rm -f "$TMP"

erlc "${M}.erl"

rm "generate.beam"
rm "generate_map.beam"
rm "generate_capital_small_remove_accent.beam"

popd

