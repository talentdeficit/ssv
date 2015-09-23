%% @hidden
-module(ssv_read).
-export([read/2]).


-record(read_opts, {
  remove_header = false,
  separator = <<",">>
}).

-spec read(binary(), ssv:read_opts()) -> [tuple()].

read(SSV, OptList) ->
  Opts = parse_opts(OptList, #read_opts{}),
  start(SSV, Opts).

parse_opts([remove_header|Rest], Opts) ->
  parse_opts(Rest, Opts#read_opts{remove_header=true});
parse_opts([], Opts) -> Opts.

start(SSV, Opts) -> rows(SSV, Opts, []).

rows(<<>>, _Opts, Acc) -> lists:reverse(Acc);
rows(SSV, Opts, Acc) ->
  case row(SSV, Opts, []) of
    {drop, Rest} -> rows(Rest, Opts#read_opts{remove_header=false}, Acc);
    {Row, Rest}  -> rows(Rest, Opts, [Row|Acc])
  end.

row(<<>>, #read_opts{remove_header=true}, _Acc) -> {drop, <<>>};
row(<<>>, _Opts, Acc) -> {row_to_tuple(Acc), <<>>};
row(<<"\n"/utf8, Rest/binary>>, #read_opts{remove_header=true}, _Acc) -> {drop, Rest};
row(<<"\n"/utf8, Rest/binary>>, _Opts, Acc) -> {row_to_tuple(Acc), Rest};
%% leading comma
row(<<","/utf8, _/binary>> = Bin, Opts, []) ->
  row(Bin, Opts, [<<>>]);
row(<<","/utf8, Rest/binary>>, Opts, Acc) ->
  {Field, More} = field(Rest, Opts),
  row(More, Opts, [Field|Acc]);
row(SSV, Opts, Acc) ->
  {Field, Rest} = field(SSV, Opts),
  row(Rest, Opts, [Field|Acc]).

row_to_tuple(Row) -> list_to_tuple(lists:reverse(Row)).

field(<<"\""/utf8, Rest/binary>>, Opts) -> delimited_field(Rest, Opts, []);
field(Bin, Opts) -> plain_field(Bin, Opts, []).

delimited_field(<<>>, _Opts, Acc) -> {unicode:characters_to_binary(Acc, utf8), <<>>};
delimited_field(<<"\"\""/utf8, Rest/binary>>, Opts, Acc) -> delimited_field(Rest, Opts, [Acc, <<"\"">>]);
delimited_field(<<"\""/utf8, Rest/binary>>, _Opts, Acc) -> {unicode:characters_to_binary(Acc, utf8), Rest};
delimited_field(<<C/utf8, Rest/binary>>, Opts, Acc) -> delimited_field(Rest, Opts, [Acc, C]).

plain_field(<<>>, _Opts, Acc) -> {unicode:characters_to_binary(Acc, utf8), <<>>};
plain_field(<<","/utf8, _/binary>> = Bin, _Opts, Acc) -> {unicode:characters_to_binary(Acc, utf8), Bin};
plain_field(<<"\n"/utf8, _/binary>> = Bin, _Opts, Acc) -> {unicode:characters_to_binary(Acc, utf8), Bin};
plain_field(<<C/utf8, Rest/binary>>, Opts, Acc) -> plain_field(Rest, Opts, [Acc, C]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

read_test_() ->
  [
    {"empty csv", ?_assertEqual([], read(<<>>, []))},
    {"empty row", ?_assertEqual([{<<"1">>,<<"2">>},{},{<<"3">>,<<"4">>}], read(<<"1,2\n\n3,4">>, []))},
    {"empty field", ?_assertEqual([{<<"1">>,<<>>,<<"2">>},{},{<<"3">>,<<"4">>,<<>>}], read(<<"1,,2\n\n3,4,">>, []))},
    {"empty quoted field", ?_assertEqual([{<<>>, <<"1">>, <<>>}], read(<<"\"\",\"1\",\"\"">>, []))},
    {"all empty field", ?_assertEqual([{<<>>,<<>>,<<>>},{<<>>,<<>>,<<>>}], read(<<",,\n,,">>, []))}
  ].

read_plain_test_() ->
  [
    {"single row", ?_assertEqual([{<<"1">>,<<"2">>,<<"3">>}], read(<<"1,2,3\n">>, []))},
    {"three rows", ?_assertEqual(
      [{<<"1">>,<<"2">>,<<"3">>},{<<"4">>,<<"5">>,<<"6">>},{<<"7">>,<<"8">>,<<"9">>}],
      read(<<"1,2,3\n4,5,6\n7,8,9\n">>, [])
    )},
    {"leading whitespace", ?_assertEqual(
      [{<<" 1">>,<<"2">>,<<"3">>},{<<"4">>,<<" 5">>,<<"6">>},{<<"7">>,<<"8">>,<<" 9">>}],
      read(<<" 1,2,3\n4, 5,6\n7,8, 9\n">>, [])
    )},
    {"trailing whitespace", ?_assertEqual(
      [{<<"1 ">>,<<"2">>,<<"3">>},{<<"4">>,<<"5 ">>,<<"6">>},{<<"7">>,<<"8">>,<<"9 ">>}],
      read(<<"1 ,2,3\n4,5 ,6\n7,8,9 \n">>, [])
    )},
    {"variable length rows", ?_assertEqual(
      [{<<"1">>,<<"2">>,<<"3">>},{<<"4">>,<<"5">>},{<<"6">>}],
      read(<<"1,2,3\n4,5\n6">>, [])
    )},
    {"leading comma", ?_assertEqual(
      [{<<>>,<<"1">>,<<"2">>},{<<>>,<<"3">>,<<"4">>}],
      read(<<",1,2\n,3,4">>, [])
    )},
    {"trailing comma", ?_assertEqual(
      [{<<"1">>,<<"2">>,<<>>},{<<"3">>,<<"4">>,<<>>}],
      read(<<"1,2,\n3,4,">>, [])
    )}
  ].

read_delimited_test_() ->
  [
    {"single row", ?_assertEqual([{<<"1">>,<<"2">>,<<"3">>}], read(<<"\"1\",\"2\",\"3\"\n">>, []))},
    {"three rows", ?_assertEqual(
      [{<<"1">>,<<"2">>,<<"3">>},{<<"4">>,<<"5">>,<<"6">>},{<<"7">>,<<"8">>,<<"9">>}],
      read(<<"\"1\",\"2\",\"3\"\n\"4\",\"5\",\"6\"\n\"7\",\"8\",\"9\"\n">>, [])
    )},
    {"leading whitespace", ?_assertEqual(
      [{<<" 1">>,<<"2">>,<<"3">>},{<<"4">>,<<" 5">>,<<"6">>},{<<"7">>,<<"8">>,<<" 9">>}],
      read(<<"\" 1\",\"2\",\"3\"\n\"4\",\" 5\",\"6\"\n\"7\",\"8\",\" 9\"\n">>, [])
    )},
    {"trailing whitespace", ?_assertEqual(
      [{<<"1 ">>,<<"2">>,<<"3">>},{<<"4">>,<<"5 ">>,<<"6">>},{<<"7">>,<<"8">>,<<"9 ">>}],
      read(<<"\"1 \",\"2\",\"3\"\n\"4\",\"5 \",\"6\"\n\"7\",\"8\",\"9 \"\n">>, [])
    )},
    {"escaped quote", ?_assertEqual(
      [{<<"\"1">>,<<"2">>,<<"3">>}],
      read(<<"\"\"\"1\",\"2\",\"3\"\n">>, [])
    )},
    {"escaped quote + whitespace", ?_assertEqual(
      [{<<" \" 1">>,<<"2">>,<<"3">>}],
      read(<<"\" \"\" 1\",\"2\",\"3\"\n">>, [])
    )},
    {"variable length rows", ?_assertEqual(
      [{<<"1">>,<<"2">>,<<"3">>},{<<"4">>,<<"5">>},{<<"6">>}],
      read(<<"\"1\",\"2\",\"3\"\n\"4\",\"5\"\n\"6\"\n">>, [])
    )},
    {"leading comma", ?_assertEqual(
      [{<<>>,<<"1">>,<<"2">>},{<<>>,<<"3">>,<<"4">>}],
      read(<<",\"1\",\"2\"\n,\"3\",\"4\"">>, [])
    )},
    {"trailing comma", ?_assertEqual(
      [{<<"1">>,<<"2">>,<<>>},{<<"3">>,<<"4">>,<<>>}],
      read(<<"\"1\",\"2\",\n\"3\",\"4\",">>, [])
    )}
  ].

header_test_() ->
  [
    {"remove header", ?_assertEqual(
      [{<<"1">>,<<"2">>,<<"3">>},{<<"4">>,<<"5">>,<<"6">>}],
      read(<<"integer,integer,integer\n1,2,3\n4,5,6">>, [remove_header])
    )}
  ].

-endif.