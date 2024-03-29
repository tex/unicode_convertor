% Copyright (c) 2012 Milan Svoboda
% See MIT-LICENSE for licensing information.

-module(generate).

-export([generate/1, run/2, output_map/1, output_capital_small/1, output_remove_accent/1]).

generate(D) ->
	case io:get_line('') of
		eof ->
            D;
        [$#|R] ->
            generate(
                case R of
                    "NAME:" ++ Name ->
                        case string:to_lower(string:strip(Name, right, $\n)) of
                            "iso-8859-" ++ Id -> 
                                dict:append(name, "iso-8859-" ++ Id, dict:erase(name, D));
                            "cp" ++ Id ->
                                dict:append(name, "cp" ++ Id, dict:erase(name, D))
                        end;
                    _ ->
                        D
                end );
        [$\n] ->
            generate(D);
        Text ->
            generate(
                case string:str(Text, "#UNDEFINED") of
                    0 ->
                        [Iso, Unicode, UnicodeName] = string:tokens(string:strip(Text, right, $\n), "\t#"),
                        parse(D, Iso, Unicode, UnicodeName);
                    _ ->
                        D
                end )
	end.

run(D, Fun) ->
    lists:foldl(
        fun (name, S) ->
                S;
            (Key = {CP, Letter, Accent}, S) ->
                Values = dict:fetch(Key, D),
                Fun(S, CP, Letter, Accent, Values)
        end, sets:new(), dict:fetch_keys(D) ).

extract_all(Fun) ->
    fun(S, CP, Letter, Accent, Values) ->
            lists:foldl(
                fun(Key, S1) ->
                        lists:foldl(
                            fun([{i, I}, {u, U}], S2) ->
                                    Fun(S2, CP, Letter, Accent, I, U)
                            end , S1, proplists:get_all_values(Key, Values) )
                end, S, proplists:get_keys(Values) )
    end.

extract_small_capital(Fun) ->
    fun(S, CP, Letter, Accent, Values) ->
            try
                [{i, SI}, {u, SU}] = proplists:get_value(small, Values),
                [{i, CI}, {u, CU}] = proplists:get_value(capital, Values),
                Fun(S, CP, Letter, Accent, SI, SU, CI, CU)
            catch
                _:_ ->
                    % io:format("ERROR: ~p~n", [Values])
                    S
            end
    end.

output_map(D) ->
    S2 = run(D, extract_all(
            fun (S, CP, _Letter, _Accent, I, U) ->
                    S1 = sets:add_element(io_lib:format("m(unicode, {'~s', 16#~s}) -> 16#~s;", [CP, I, U]), S),
                    sets:add_element(io_lib:format("m('~s', {unicode, 16#~s}) -> 16#~s;", [CP, U, I]), S1)
            end )),
    [io:format("~s~n", [X]) || X <- sets:to_list(S2)],
    io:format("m(_, {_, _}) -> throw(badarg).~n").

output_remove_accent(D) ->
    S4 = run(D, extract_small_capital(
            fun (S, _CP, _Letter, none, _SI, _SU, _CI, _CU) ->
                    S;
                (S, CP, Letter, _Accent, SI, SU, CI, CU) ->
                    Target = dict:fetch({CP, Letter, none}, D),
                    [{i, TSI}, {u, TSU}] = proplists:get_value(small, Target),
                    [{i, TCI}, {u, TCU}] = proplists:get_value(capital, Target),

                    S1 = sets:add_element(io_lib:format("r({'~s', 16#~s}) -> 16#~s;", [CP, SI, TSI]), S),
                    S2 = sets:add_element(io_lib:format("r({'~s', 16#~s}) -> 16#~s;", [CP, CI, TCI]), S1),
                    S3 = sets:add_element(io_lib:format("r({unicode, 16#~s}) -> 16#~s;", [SU, TSU]), S2),
                    sets:add_element(io_lib:format("r({unicode, 16#~s}) -> 16#~s;", [CU, TCU]), S3)
            end )),
    [io:format("~s~n", [X]) || X <- sets:to_list(S4)],
    io:format("r({_, L}) -> L.~n").

output_capital_small(D) ->
    S2 = run(D, extract_small_capital(
            fun(S, CP, _Letter, _Accent, SI, SU, CI, CU) ->
                    S1 = sets:add_element(io_lib:format("c({'~s', 16#~s}) -> 16#~s;", [CP, SI, CI]), S),
                    sets:add_element(io_lib:format("c({unicode, 16#~s}) -> 16#~s;", [SU, CU]), S1)
            end )),
    [io:format("~s~n", [X]) || X <- sets:to_list(S2)],
    io:format("c({_, L}) -> L.~n"),

    S6 = run(D, extract_small_capital(
            fun(S4, CP, _Letter, _Accent, SI, SU, CI, CU) ->
                    S5 = sets:add_element(io_lib:format("s({'~s', 16#~s}) -> 16#~s;", [CP, CI, SI]), S4),
                    sets:add_element(io_lib:format("s({unicode, 16#~s}) -> 16#~s;", [CU, SU]), S5)
            end )),
    [io:format("~s~n", [X]) || X <- sets:to_list(S6)],
    io:format("s({_, L}) -> L.~n").

parse_latin(D, Iso, Unicode, UnicodeName, Case) ->
    Letter = string:to_lower(lists:nth(1, UnicodeName)),
    Rest = lists:nthtail(1, UnicodeName),
    CP = dict:fetch(name, D),

    Append = {Case, [{i, Iso}, {u, Unicode}]},
    case Rest of
        []                 -> dict:append({CP, Letter, none}, Append, D);
        " WITH " ++ Accent -> dict:append({CP, Letter, Accent}, Append, D);
        _                  -> dict:append({CP, Letter, other}, Append, D)
    end.

parse(D, "0x" ++ Iso, "0x" ++ Unicode, "LATIN SMALL LETTER " ++ UnicodeName) ->
    parse_latin(D, Iso, Unicode, UnicodeName, small);

parse(D, "0x" ++ Iso, "0x" ++ Unicode, "LATIN CAPITAL LETTER " ++ UnicodeName) ->
    parse_latin(D, Iso, Unicode, UnicodeName, capital);

parse(D, "0x" ++ Iso, "0x" ++ Unicode, UnicodeName) ->
    CP = dict:fetch(name, D),
    dict:append({CP, UnicodeName, other}, {other, [{i, Iso}, {u, Unicode}]}, D);

parse(D, _, _, _) ->
    D.

