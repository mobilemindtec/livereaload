-module(livereload_handler_index).

-export([
	init/2
]).

init(Req0, State) ->
  Req = cowboy_req:reply(200,
      #{<<"content-type">> => <<"text/plain">>},
      <<"alive">>,
      Req0),
  {ok, Req, State}.