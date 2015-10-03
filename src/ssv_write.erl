%% @hidden
-module(ssv_write).
-export([write/2]).


-record(write_opts, {
  separator = <<",">>,
  escape = auto
}).

-spec write([tuple()], ssv:write_opts()) -> [tuple()].

write(Tuples, OptList) ->
  Opts = parse_opts(OptList, #write_opts{}),
  lists:foldl(fun(Row, Acc) -> <<Acc/binary, (tuple_to_row(Row, Opts))/binary>> end, <<>>, Tuples).

parse_opts([], Opts) -> Opts;
parse_opts([{separator, comma}|Rest], Opts) -> parse_opts(Rest, Opts#write_opts{separator = <<",">>});
parse_opts([{separator, tab}|Rest], Opts) -> parse_opts(Rest, Opts#write_opts{separator = <<"\t">>});
parse_opts([{separator, pipe}|Rest], Opts) -> parse_opts(Rest, Opts#write_opts{separator = <<"|">>});
parse_opts([{separator, Sep}|Rest], Opts) -> parse_opts(Rest, Opts#write_opts{separator = Sep});
parse_opts([{escape, auto}|Rest], Opts) -> parse_opts(Rest, Opts#write_opts{escape = auto});
parse_opts([{escape, always}|Rest], Opts) -> parse_opts(Rest, Opts#write_opts{escape = always});
parse_opts([{escape, never}|Rest], Opts) -> parse_opts(Rest, Opts#write_opts{escape = never}). 

tuple_to_row({}, _Opts) -> <<"\r\n">>;
tuple_to_row(Tuple, Opts) ->
  Terms = tuple_to_list(Tuple),
  Fields = lists:map(fun(Term) -> format_field(Term, Opts) end, Terms),
  Row = join(Fields, Opts#write_opts.separator),
  <<Row/binary, $\r, $\n/utf8>>.

format_field(Field, Opts) when is_binary(Field) -> escape_field(Field, Opts);
format_field(Field, _Opts) when is_integer(Field) -> integer_to_binary(Field);
format_field(Field, _Opts) when is_float(Field) -> float_to_binary(Field);
format_field(Field, _Opts) when is_atom(Field) -> atom_to_binary(Field, utf8).

escape_field(Field, #write_opts{escape = never}) -> Field;
escape_field(Field, Opts) ->
  case escape_field(Field, <<>>, clean, Opts) of
    {clean, _Clean} -> Field;
    {dirty, Dirty}  -> <<$\"/utf8, Dirty/binary, $\"/utf8>>
  end.

escape_field(<<>>, Acc, Status, Opts) ->
  case Opts#write_opts.escape of
    always -> {dirty, Acc};
    auto   -> {Status, Acc}
  end;
escape_field(<<$\"/utf8, Rest/binary>>, Acc, Status, Opts) ->
  escape_field(Rest, <<Acc/binary, $\"/utf8, $\"/utf8>>, Status, Opts);
escape_field(<<$,/utf8, Rest/binary>>, Acc, _Status, Opts) ->
  escape_field(Rest, <<Acc/binary, $,/utf8>>, dirty, Opts);
escape_field(<<$\r/utf8, $\n/utf8, Rest/binary>>, Acc, _Status, Opts) ->
  escape_field(Rest, <<Acc/binary, $\r/utf8, $\n/utf8>>, dirty, Opts);
escape_field(<<C/utf8, Rest/binary>>, Acc, Status, Opts) ->
  escape_field(Rest, <<Acc/binary, C/utf8>>, Status, Opts).

join([], _Sep) -> [];
join([H|T], Sep) ->
  Rows = lists:foldl(fun(Field, Acc) -> <<Acc/binary, Sep/binary, Field/binary>> end, <<>>, T),
  <<H/binary, Rows/binary>>.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

write_test_() ->
  [
    {"empty csv", ?_assertEqual(<<>>, write([], []))},
    {"empty row", ?_assertEqual(<<"1,2\r\n\r\n3,4\r\n">>, write([{<<"1">>,<<"2">>},{},{<<"3">>,<<"4">>}], []))},
    {"empty field", ?_assertEqual(<<"1,,2\r\n\r\n3,4,\r\n">>, write([{<<"1">>,<<>>,<<"2">>},{},{<<"3">>,<<"4">>,<<>>}], []))},
    {"all empty field", ?_assertEqual(<<",,\r\n,,\r\n">>, write([{<<>>,<<>>,<<>>},{<<>>,<<>>,<<>>}], []))}
  ].

escape_test_() ->
  [
    {"clean fields", ?_assertEqual(<<"a,b,c\r\nd,e,f\r\n">>, write([{<<"a">>,<<"b">>,<<"c">>},{<<"d">>,<<"e">>,<<"f">>}], []))},
    {"comma", ?_assertEqual(
      <<"\",leading comma\",\"trailing comma,\",\"embedded, comma\"\r\n">>,
      write([{<<",leading comma">>,<<"trailing comma,">>,<<"embedded, comma">>}], []) 
    )},
    {"newline", ?_assertEqual(
      <<"\"\r\nleading newline\",\"trailing newline\r\n\",\"embedded\r\nnewline\"\r\n">>,
      write([{<<"\r\nleading newline">>,<<"trailing newline\r\n">>,<<"embedded\r\nnewline">>}], []) 
    )},
    {"unescaped quote", ?_assertEqual(
      <<"1\",\"2,3\"4\r\n">>,
      write([{<<"1\"">>,<<"\"2">>,<<"3\"4">>}], [])
    )},
    {"always escape", ?_assertEqual(
      <<"\"a\",\"b\",\"c\"\r\n\"d\",\"e\",\"f\"\r\n">>,
      write([{<<"a">>,<<"b">>,<<"c">>},{<<"d">>,<<"e">>,<<"f">>}], [{escape, always}])
    )},
    {"never escape", ?_assertEqual(
      <<"never, escape,even\r\nwhen,necessary\r\n">>,
      write([{<<"never, escape">>,<<"even\r\nwhen">>,<<"necessary">>}], [{escape, never}])
    )}
  ].

alternate_separator_test_() ->
  [
    {"comma separated", ?_assertEqual(<<"a,b,c\r\n">>, write([{<<"a">>,<<"b">>,<<"c">>}], [{separator, <<",">>}]))},
    {"comma separated", ?_assertEqual(<<"a,b,c\r\n">>, write([{<<"a">>,<<"b">>,<<"c">>}], [{separator, comma}]))},
    {"tab separated", ?_assertEqual(<<"a\tb\tc\r\n">>, write([{<<"a">>,<<"b">>,<<"c">>}], [{separator, <<"\t">>}]))},
    {"tab separated", ?_assertEqual(<<"a\tb\tc\r\n">>, write([{<<"a">>,<<"b">>,<<"c">>}], [{separator, tab}]))},
    {"pipe separated", ?_assertEqual(<<"a|b|c\r\n">>, write([{<<"a">>,<<"b">>,<<"c">>}], [{separator, <<"|">>}]))},
    {"pipe separated", ?_assertEqual(<<"a|b|c\r\n">>, write([{<<"a">>,<<"b">>,<<"c">>}], [{separator, pipe}]))}
  ].

-endif.