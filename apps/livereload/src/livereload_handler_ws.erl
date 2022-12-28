-module(livereload_handler_ws).

-export([init/2, websocket_init/1]).
-export([websocket_handle/3, websocket_handle/2, websocket_info/3, websocket_info/2, terminate/3]).

-record(state, { path :: string(), 
				 exts :: list() }).

find_key(KeyName, Vals, Default) ->
	case lists:keysearch(KeyName, 1, Vals) of
		false -> Default;
		{value, {_, Val}} -> Val
	end.

init(Req, _Opts) ->
	QsVals = cowboy_req:parse_qs(Req),
	Path = find_key(<<"path">>, QsVals, undefined),
	Exts0 = find_key(<<"exts">>, QsVals, ""),
	Exts = [binary_to_list(X) || X <- string:split(Exts0, ",")],
	lager:info("--:::livereload_handler_ws:::-- [init] init with path: ~p, exts: ~p", [Path, Exts]),
	State = #state{ path = binary_to_list(Path), exts = Exts },
	{cowboy_websocket, Req, State, #{idle_timeout => 60 * 1000 * 60}}.

websocket_init(State) ->
	lager:info("--:::livereload_handler_ws:::-- [websocket_init] start"),
	case State of
		#state{ path = undefined } ->
			lager:info("--:::livereload_handler_ws:::-- [websocket_init] path undefined"),
			exit(self(), "path undefined");
		#state{ path = Path } ->			
				case filelib:is_dir(Path) of
					false -> 
						lager:info("--:::livereload_handler_ws:::-- [websocket_init] path not found or is not a dir"),
						exit(self(), "path not found or is not a dir");
					true ->
						lager:info("--:::livereload_handler_ws:::-- [websocket_init] watching files at ~p", [Path]),
						fs:start_link(fs_watcher, Path),
						fs:subscribe(fs_watcher),
						ok
				end
	end,
	lager:info("--:::livereload_handler_ws:::-- [websocket_init] connected!"),
	{ok, State}.

websocket_handle(Data, Req, State) ->
	lager:info("--:::livereload_handler_ws:::-- [websocket_handle] received ~p", [Data]),
	{ok, Req, State}.

websocket_handle({text, MessageData}, State) ->	
	Resp = {text, <<"your message was received">>},
	{reply, Resp, State}.

websocket_info({msg, Message}, State) ->	
	{reply, {text, <<"">>}, State};

websocket_info(Info, #state{ exts = Exts }=State) ->

	case Info of 
		{_, _, {FilePath, _}} ->

			lager:info("--:::livereload_handler_ws:::-- [websocket_info] file changed ~p", [FilePath]),

			SplitedPath = string:split(FilePath, "/"),
			FileName = lists:nthtail(length(SplitedPath)-1, SplitedPath),
			SplitedExts = string:split(FileName, "."),
			FileExt = lists:nthtail(length(SplitedExts)-1, SplitedExts),

			Found = case Exts of
				["*"] -> true;
				_ -> 
					Filtered = lists:filter(fun(Ext) ->  Ext =:= FileExt end, Exts),
					length(Filtered) > 0
			end,
			
			case Found of
				true -> 
					ReloadMsg = #{event => <<"reload">>},
					{reply, {text, jsx:encode(ReloadMsg)}, State};
				_ ->
					lager:info("--:::livereload_handler_ws:::-- [websocket_info] ignore file change"),
					{ok, State}
			end;
		_ ->		
    		lager:info("--:::livereload_handler_ws:::-- [websocket_info] ignore unknown message: ~p", [Info]),
    		{ok, State}
	end.   

websocket_info(Info, _Req, _State) ->
	lager:info("--:::livereload_handler_ws:::-- [websocket_info] unknown message ~p", [Info]).

terminate(Info, _Req, State) ->
	lager:info("--:::livereload_handler_ws:::-- [websocket_terminate] terminating websocket ~p, ~p", [Info, State]),
	ok.
