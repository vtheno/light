-module(light).
-export([start/1, pre_connect/2, do_recv/2, parse/1, stream_format/1]).

start(Handler) ->
    ssl:start(), 
    {ok, ServerSock} = ssl:listen(443, [
					{cacertfile, "certs.pem"},
					{certfile, "ca.pem"}, 
					{keyfile, "key.pem"},
					{reuseaddr, true}
				       ]),
    io:format("Server Start by: ~p~n", [ServerSock]),
    spawn(?MODULE, pre_connect, [ServerSock, Handler]).

pre_connect(ServerSock, Handler) ->
    case ssl:transport_accept(ServerSock) of
	{ok, TransportSock} ->
	    spawn(?MODULE, pre_connect, [ServerSock, Handler]),
	    case ssl:handshake(TransportSock) of
		{ok, ClientSock} ->
		    do_recv(ClientSock, Handler);
		{error, Msg} ->
		    io:format("Handshake Error: ~p~n", [Msg])
	    end;
	{error, Msg} ->
	    io:format("Transport Error: ~p~n", [Msg])
    end.

do_recv(ClientSock, Handler) ->
    %% io:format("~p~n",[ClientSock]),
    receive
	{ssl, _, Binary} -> % _ is Socket
	    case parse(Binary) of
		{ok, Data} -> 
		    #{uri := Uri, method:= Method} = Data,
		    io:format("~s ~s~n", [Method, Uri]),
		    case ssl:send(ClientSock, stream_format(Handler(Data))) of
			ok ->
			    %% io:format("send.~n"),
			    ok = ssl:close(ClientSock);
			{error, _} ->
			    ok = ssl:close(ClientSock)
		    end;
		{error, _} -> 
		    ok = ssl:close(ClientSock),
		    error
	    end;
	{ssl_close, _} -> % _ is Socket
	    io:format("ssl close ssl."),
	    ok = ssl:close(ClientSock);
	{ssl_error, _, Msg} -> % _ is Socket
	    io:format("SSL ERROR: ~p~n", [Msg])
    end,
    do_recv(ClientSock, Handler).

%% datatype msg = ok | error
%% parse : string -> msg * map
%% map is dict
%% InfoTable : orddict
parse(String) when is_list(String) ->
    [Head|[Form]] = string:split(String, "\r\n\r\n"),
    [Header|[Info]] = string:split(Head, "\r\n"),
    [Method|[Uri|[Version]]] = string:tokens(Header, " "),
    InfoTable = [list_to_tuple(string:split(X, ": ")) || X <- string:tokens(Info, "\r\n")],
    %% {Value, _} = orddict:take("Host", InfoTable),
    %% io:format("~p~n", [Value]),
    {ok,
     #{ 
	method => Method,
	uri => Uri,
	version => Version,
	form => Form,
	info => InfoTable
      }
    };
parse(_) ->
    {error, #{}}.

stream_format([Head|Tail]) -> Head ++ "\r\n" ++ stream_format(Tail);
stream_format([]) -> [].
