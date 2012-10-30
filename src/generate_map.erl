% Copyright (c) 2012 Milan Svoboda
% See MIT-LICENSE for licensing information.

-module(generate_map).

-export([generate_map/0]).

generate_map() ->
    D = generate:generate(dict:new()),
    generate:output_map(D),
    init:stop().
