%%%-------------------------------------------------------------------
%%% @author Dmitry Davidenko
%%% @copyright (C) 2017
%%% @doc
%%% @end
%%% Created : 05. Jun 2017 14:12
%%%-------------------------------------------------------------------

-type esprink_session_type() :: file.
-type esprink_session_status() :: active.

-record(esprink_session, {
    id :: binary(),
    source_type = file :: esprink_session_type(), %% The only one source type is supported currently
    status = active :: esprink_session_status(), %% The only one status is supported currently
    options = #{} :: #{}
}).