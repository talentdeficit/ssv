%% @hidden
-module(ssv_read).
-export([read/2]).


-record(read_opts, {
  remove_header = false
}).

-spec read(binary(), ssv:read_opts()) -> [tuple()].

read(SSV, OptList) ->
  Opts = parse_opts(OptList, #read_opts{}),
  start(SSV, Opts).

parse_opts([], Opts) -> Opts;
parse_opts([remove_header|Rest], Opts) -> parse_opts(Rest, Opts#read_opts{remove_header = true}).

start(SSV, Opts) -> rows(SSV, Opts, []).

rows(<<>>, _Opts, Acc) -> lists:reverse(Acc);
rows(SSV, Opts, Acc) ->
  case row(SSV, Opts, []) of
    {drop, Rest} -> rows(Rest, Opts#read_opts{remove_header = false}, Acc);
    {Row, Rest}  -> rows(Rest, Opts, [Row|Acc])
  end.

row(<<>>, #read_opts{remove_header = true}, _Acc) -> {drop, <<>>};
row(<<>>, _Opts, Acc) -> {row_to_tuple(Acc), <<>>};
row(<<"\r\n"/utf8, Rest/binary>>, #read_opts{remove_header = true}, _Acc) -> {drop, Rest};
row(<<"\r\n"/utf8, Rest/binary>>, _Opts, Acc) -> {row_to_tuple(Acc), Rest};
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
plain_field(<<"\r\n"/utf8, _/binary>> = Bin, _Opts, Acc) -> {unicode:characters_to_binary(Acc, utf8), Bin};
plain_field(<<C/utf8, Rest/binary>>, Opts, Acc) -> plain_field(Rest, Opts, [Acc, C]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

read_test_() ->
  [
    {"empty csv", ?_assertEqual([], read(<<>>, []))},
    {"empty row", ?_assertEqual([{<<"1">>,<<"2">>},{},{<<"3">>,<<"4">>}], read(<<"1,2\r\n\r\n3,4">>, []))},
    {"empty field", ?_assertEqual([{<<"1">>,<<>>,<<"2">>},{},{<<"3">>,<<"4">>,<<>>}], read(<<"1,,2\r\n\r\n3,4,">>, []))},
    {"empty quoted field", ?_assertEqual([{<<>>, <<"1">>, <<>>}], read(<<"\"\",\"1\",\"\"">>, []))},
    {"all empty field", ?_assertEqual([{<<>>,<<>>,<<>>},{<<>>,<<>>,<<>>}], read(<<",,\r\n,,">>, []))}
  ].

read_plain_test_() ->
  [
    {"single row", ?_assertEqual([{<<"1">>,<<"2">>,<<"3">>}], read(<<"1,2,3\r\n">>, []))},
    {"three rows", ?_assertEqual(
      [{<<"1">>,<<"2">>,<<"3">>},{<<"4">>,<<"5">>,<<"6">>},{<<"7">>,<<"8">>,<<"9">>}],
      read(<<"1,2,3\r\n4,5,6\r\n7,8,9\r\n">>, [])
    )},
    {"leading whitespace", ?_assertEqual(
      [{<<" 1">>,<<"2">>,<<"3">>},{<<"4">>,<<" 5">>,<<"6">>},{<<"7">>,<<"8">>,<<" 9">>}],
      read(<<" 1,2,3\r\n4, 5,6\r\n7,8, 9\r\n">>, [])
    )},
    {"trailing whitespace", ?_assertEqual(
      [{<<"1 ">>,<<"2">>,<<"3">>},{<<"4">>,<<"5 ">>,<<"6">>},{<<"7">>,<<"8">>,<<"9 ">>}],
      read(<<"1 ,2,3\r\n4,5 ,6\r\n7,8,9 \r\n">>, [])
    )},
    {"variable length rows", ?_assertEqual(
      [{<<"1">>,<<"2">>,<<"3">>},{<<"4">>,<<"5">>},{<<"6">>}],
      read(<<"1,2,3\r\n4,5\r\n6">>, [])
    )},
    {"leading comma", ?_assertEqual(
      [{<<>>,<<"1">>,<<"2">>},{<<>>,<<"3">>,<<"4">>}],
      read(<<",1,2\r\n,3,4">>, [])
    )},
    {"trailing comma", ?_assertEqual(
      [{<<"1">>,<<"2">>,<<>>},{<<"3">>,<<"4">>,<<>>}],
      read(<<"1,2,\r\n3,4,">>, [])
    )}
  ].

read_delimited_test_() ->
  [
    {"single row", ?_assertEqual([{<<"1">>,<<"2">>,<<"3">>}], read(<<"\"1\",\"2\",\"3\"\r\n">>, []))},
    {"three rows", ?_assertEqual(
      [{<<"1">>,<<"2">>,<<"3">>},{<<"4">>,<<"5">>,<<"6">>},{<<"7">>,<<"8">>,<<"9">>}],
      read(<<"\"1\",\"2\",\"3\"\r\n\"4\",\"5\",\"6\"\r\n\"7\",\"8\",\"9\"\r\n">>, [])
    )},
    {"leading whitespace", ?_assertEqual(
      [{<<" 1">>,<<"2">>,<<"3">>},{<<"4">>,<<" 5">>,<<"6">>},{<<"7">>,<<"8">>,<<" 9">>}],
      read(<<"\" 1\",\"2\",\"3\"\r\n\"4\",\" 5\",\"6\"\r\n\"7\",\"8\",\" 9\"\r\n">>, [])
    )},
    {"trailing whitespace", ?_assertEqual(
      [{<<"1 ">>,<<"2">>,<<"3">>},{<<"4">>,<<"5 ">>,<<"6">>},{<<"7">>,<<"8">>,<<"9 ">>}],
      read(<<"\"1 \",\"2\",\"3\"\r\n\"4\",\"5 \",\"6\"\r\n\"7\",\"8\",\"9 \"\r\n">>, [])
    )},
    {"escaped quote", ?_assertEqual(
      [{<<"\"1">>,<<"2">>,<<"3">>}],
      read(<<"\"\"\"1\",\"2\",\"3\"\r\n">>, [])
    )},
    {"escaped quote + whitespace", ?_assertEqual(
      [{<<" \" 1">>,<<"2">>,<<"3">>}],
      read(<<"\" \"\" 1\",\"2\",\"3\"\r\n">>, [])
    )},
    {"variable length rows", ?_assertEqual(
      [{<<"1">>,<<"2">>,<<"3">>},{<<"4">>,<<"5">>},{<<"6">>}],
      read(<<"\"1\",\"2\",\"3\"\r\n\"4\",\"5\"\r\n\"6\"\r\n">>, [])
    )},
    {"leading comma", ?_assertEqual(
      [{<<>>,<<"1">>,<<"2">>},{<<>>,<<"3">>,<<"4">>}],
      read(<<",\"1\",\"2\"\r\n,\"3\",\"4\"">>, [])
    )},
    {"trailing comma", ?_assertEqual(
      [{<<"1">>,<<"2">>,<<>>},{<<"3">>,<<"4">>,<<>>}],
      read(<<"\"1\",\"2\",\r\n\"3\",\"4\",">>, [])
    )}
  ].

header_test_() ->
  [
    {"remove header", ?_assertEqual(
      [{<<"1">>,<<"2">>,<<"3">>},{<<"4">>,<<"5">>,<<"6">>}],
      read(<<"integer,integer,integer\r\n1,2,3\r\n4,5,6">>, [remove_header])
    )}
  ].

-endif.