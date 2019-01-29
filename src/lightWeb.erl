-module(lightWeb).
-export([start/1, pre_connect/2, do_recv/2, parse/1, stream_format/1]).

start(Handler) ->
    {ok, ServerSock} = gen_tcp:listen(80, [binary,
					   {packet, 0},
					   {active, true},
					   {reuseaddr, true}
					  ]),
    io:format("Server Start by: ~p~n", [ServerSock]),
    %% spawn(?MODULE, pre_connect, [ServerSock, Handler]).
    spawn(?MODULE, pre_connect, [ServerSock, Handler]).


pre_connect(ServerSock, Handler) ->
    case gen_tcp:accept(ServerSock) of
	{ok, ClientSock} ->
	    do_recv(ClientSock, Handler),
	    spawn(?MODULE, pre_connect, [ServerSock, Handler]);
	{error, Msg} ->
	    io:format("Error: ~p~n", [Msg])
    end.

do_recv(ClientSock, Handler) ->
    receive
	{tcp, ClientSock, Binary} ->
	    case parse(binary_to_list(Binary)) of
		{ok, Data} -> 
		    %% io:format("~p~n", [Data]),
		    case gen_tcp:send(ClientSock, Handler(Data)) of
			ok ->
			    ok = gen_tcp:close(ClientSock);
			{error, _} -> 
			    ok = gen_tcp:close(ClientSock)
		    end;
		{error, _} -> 
		    ok = gen_tcp:close(ClientSock),
		    error
	    end;
	{tcp_close, ClientSock} ->
	    ok = gen_tcp:close(ClientSock),
	    io:format("server close!")
    end.

%% datatype msg = ok | error
%% parse : string -> msg * map
%% map is dict
parse(String) when is_list(String) ->
    [Head|[Form]] = string:split(String, "\r\n\r\n"),
    [Header|[Info]] = string:split(Head, "\r\n"),
    [Method|[Uri|[Version]]] = string:tokens(Header, " "),
    {ok,
     #{ 
	method => Method,
	uri => Uri,
	version => Version,
	form => Form,
	info => Info
      }
    };
parse(_) ->
    {error, #{}}.

stream_format([Head|Tail]) -> Head ++ "\r\n" ++ stream_format(Tail);
stream_format([]) -> [].
