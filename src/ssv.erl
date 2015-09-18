%% @author alisdair sullivan [https://github.com/talentdeficit]
%% @copyright 2015 alisdair sullivan
%% @doc
%% a csv reading and writing framework
%% @version 0.1.0
-module(ssv).
-export([read/1, read/2, write/1, write/2]).


-type read_opts() :: [read_opt()].
-type read_opt() :: drop_header.

-export_type([read_opts/0]).

-type write_opts() :: [].

-export_type([write_opts/0]).

%% @equiv read(SSV, []).
-spec read(binary()) -> [tuple()].

read(SSV) when is_binary(SSV) -> ssv_read:read(SSV, []).

%% @doc
%% read a utf8 binary containing csv records and output a list of erlang tuples, one
%% for each csv record
-spec read(binary(), read_opts()) -> [tuple()].

read(SSV, Opts) when is_binary(SSV) -> ssv_read:read(SSV, Opts).

%% @equiv write(Tuples, []).
-spec write([tuple()]) -> binary().

write(Tuples) when is_list(Tuples) -> ssv_write:write(Tuples, []).

%% @doc
%% write a utf8 binary containing csv records from a list of erlang tuples
-spec write([tuple()], write_opts()) -> binary().

write(Tuples, Opts) when is_list(Tuples) -> ssv_write:write(Tuples, Opts).