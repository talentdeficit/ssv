%% @hidden
-module(ssv_read).
-export([read/2]).


-record(read_opts, {
  drop = false
}).

-spec read(binary(), ssv:read_opts()) -> [tuple()].

read(SSV, OptList) ->
  Opts = parse_opts(OptList, #read_opts{}),
  start(SSV, Opts).

parse_opts([drop_header|Rest], Opts) ->
  parse_opts(Rest, Opts#read_opts{drop=true});
parse_opts([], Opts) -> Opts.

start(SSV, Opts) -> rows(SSV, Opts, []).

rows(SSV, Opts, Acc) ->
  case row(SSV, Opts, []) of
    {drop, Rest} -> rows(Rest, Opts#read_opts{drop=false}, Acc);
    {Row, Rest}  -> rows(Rest, Opts, [Row|Acc]);
    eof          -> lists:reverse(Acc)
  end.

row(<<>>, _Opts, []) -> eof;
row(SSV, Opts, Acc) ->
  case field(SSV, Opts) of
    {eor, Rest}   ->
      case Opts#read_opts.drop of
        true  -> {drop, Rest};
        false -> {row_to_tuple(Acc), Rest}
      end;
    {Field, Rest} -> row(Rest, Opts, [Field|Acc])
  end.

row_to_tuple(Row) -> list_to_tuple(lists:reverse(Row)).

field(<<>>, _Opts) -> {eor, <<>>};
field(<<"\n"/utf8, Rest/binary>>, _Opts) -> {eor, Rest};
field(<<"\""/utf8, Rest/binary>>, Opts) -> delimited_field(Rest, Opts, []);
field(Bin, Opts) -> plain_field(Bin, Opts, []).

delimited_field(<<>>, _Opts, Acc) -> {unicode:characters_to_binary(Acc, utf8), <<>>};
delimited_field(<<"\"\""/utf8, Rest/binary>>, Opts, Acc) -> delimited_field(Rest, Opts, [Acc, <<"\"">>]);
delimited_field(<<"\""/utf8, ","/utf8, Rest/binary>>, _Opts, Acc) -> {unicode:characters_to_binary(Acc, utf8), Rest};
delimited_field(<<"\""/utf8, Rest/binary>>, _Opts, Acc) -> {unicode:characters_to_binary(Acc, utf8), Rest};
delimited_field(<<C/utf8, Rest/binary>>, Opts, Acc) -> delimited_field(Rest, Opts, [Acc, C]).

plain_field(<<>>, _Opts, Acc) -> {unicode:characters_to_binary(Acc, utf8), <<>>};
plain_field(<<","/utf8, Rest/binary>>, _Opts, Acc) -> {unicode:characters_to_binary(Acc, utf8), Rest};
plain_field(<<"\n"/utf8, _/binary>> = Bin, _Opts, Acc) -> {unicode:characters_to_binary(Acc, utf8), Bin};
plain_field(<<C/utf8, Rest/binary>>, Opts, Acc) -> plain_field(Rest, Opts, [Acc, C]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

read_plain_test_() ->
  [
    {"empty csv", ?_assertEqual([], read(<<>>, []))},
    {"single row", ?_assertEqual([{<<"1">>,<<"2">>,<<"3">>}], read(<<"1,2,3\n">>, []))},
    {"three rows", ?_assertEqual(
      [{<<"1">>,<<"2">>,<<"3">>},{<<"4">>,<<"5">>,<<"6">>},{<<"7">>,<<"8">>,<<"9">>}],
      read(<<"1,2,3\n4,5,6\n7,8,9\n">>, [])
    )}
  ].

read_delimited_test_() ->
  [
    {"single row", ?_assertEqual([{<<"1">>,<<"2">>,<<"3">>}], read(<<"\"1\",\"2\",\"3\"\n">>, []))},
    {"three rows", ?_assertEqual(
      [{<<"1">>,<<"2">>,<<"3">>},{<<"4">>,<<"5">>,<<"6">>},{<<"7">>,<<"8">>,<<"9">>}],
      read(<<"\"1\",\"2\",\"3\"\n\"4\",\"5\",\"6\"\n\"7\",\"8\",\"9\"\n">>, [])
    )}
  ].

-endif.