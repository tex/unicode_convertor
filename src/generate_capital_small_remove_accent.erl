% Copyright (c) 2012 Milan Svoboda
% See MIT-LICENSE for licensing information.

-module(generate_capital_small_remove_accent).

-export([generate_capital_small_remove_accent/0]).

generate_capital_small_remove_accent() ->
    D = generate:generate(dict:new()),
    generate:output_capital_small(D),
    generate:output_remove_accent(D),
    init:stop().
