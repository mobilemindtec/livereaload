%%%-------------------------------------------------------------------
%% @doc livereload public API
%% @end
%%%-------------------------------------------------------------------

-module(livereload_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->

    lager:start(),
    lager:info("[livereload] start.."),    

    Dispatch = cowboy_router:compile(
        [{'_', [
            {"/", livereload_handler_index, []},
            {"/sample", cowboy_static, {priv_file, livereload, "sample.html"}},
            {"/ws", livereload_handler_ws, []},
            {"/[...]", cowboy_static, {priv_dir, livereload, "./"}}
        ]}]
    ),
    {ok, _} = cowboy:start_clear(livereload_listener, [{port, 10101}], #{env => #{dispatch => Dispatch}}),

    livereload_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
