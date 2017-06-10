%%%-------------------------------------------------------------------
%%% @author Dmitry Davidenko
%%% @copyright (C) 2017
%%% @doc
%%% @end
%%% Created : 10. Jun 2017 17:41
%%%-------------------------------------------------------------------
-module(esprink_file_utils).

-define(MD5_BLOCK_SIZE, 1048576). %% 2^20 byte

-include_lib("kernel/include/file.hrl").

-export([
    calculate_md5/1,
    size/1
]).

-spec calculate_md5(File :: string() | term()) -> MD5 :: string().
calculate_md5(Filename) when is_list(Filename) ->
    case file:open(Filename, [read, binary]) of
        {ok, Fd} ->
            calculate_md5(Fd);
        {error, Code} ->
            throw({md5, Code})
    end;
calculate_md5(Fd) ->
    Context = erlang:md5_init(),
    do_calculate_md5(Fd, Context).

do_calculate_md5(Fd, Context) ->
    case file:read(Fd, ?MD5_BLOCK_SIZE) of
        {ok, Data} ->
            Context2 = erlang:md5_update(Context, Data),
            do_calculate_md5(Fd, Context2);
        eof ->
            Digest = erlang:md5_final(Context),
            lists:flatten([integer_to_list(X, 16) || <<X:8>> <= Digest])
    end.

-spec size(Filename :: string()) -> non_neg_integer().
size(Filename) ->
    {ok, #file_info{size = Size}} = file:read_file_info(Filename),
    Size.