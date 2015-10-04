%% @author alisdair sullivan [https://github.com/talentdeficit]
%% @copyright 2015 alisdair sullivan
%% @doc
%% a csv reading and writing framework
%% @version 0.1.0
-module(ssv).
-export([read/1, read/2, write/1, write/2]).


-type read_opts() :: [read_opt()].
-type read_opt() :: remove_header.

-export_type([read_opts/0]).

-type write_opts() :: [write_opt()].
-type write_opt() :: {separator, binary() | tab} | {escape, auto | never | always}.

-export_type([write_opts/0]).

%% @equiv read(SSV, [])
-spec read(binary()) -> [tuple()].

read(SSV) when is_binary(SSV) -> ssv_read:read(SSV, []).

%% @doc
%% read a utf8 binary containing csv records and output a list of erlang tuples, one
%% for each csv record
%%
%% the `remove_header' option will drop the first row read from the result list
-spec read(binary(), read_opts()) -> [tuple()].

read(SSV, Opts) when is_binary(SSV) -> ssv_read:read(SSV, Opts).

%% @equiv write(Tuples, [])
-spec write([tuple()]) -> binary().

write(Tuples) when is_list(Tuples) -> ssv_write:write(Tuples, []).

%% @doc
%% write a utf8 binary containing csv records from a list of erlang tuples
%%
%% the `separator' option allows a separator other than "," to be used
%% when joining adjacent fields. the atoms `comma', `tab' and `pipe' as well
%% as any binary are valid as arguments
%%
%% the `escape' option controls the behaviour of field escaping. `auto' will
%% escape any fields with an embedded comma or CRLF and leave any other fields
%% unescaped. `never' will never escape regardless of the contents of the field.
%% `always' will always escape fields, regardless of contents
-spec write([tuple()], write_opts()) -> binary().

write(Tuples, Opts) when is_list(Tuples) -> ssv_write:write(Tuples, Opts).