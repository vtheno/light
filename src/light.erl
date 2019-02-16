-module(light).
-export([start/1, pre_connect/2, do_recv/3, parse/1]).

start(Handler) ->
    ssl:start(), 
    {ok, ServerSock} = ssl:listen(443, [
					{cacertfile, "certs.pem"},
					{certfile, "ca.pem"}, 
					{keyfile, "key.pem"},
					{reuseaddr, true},
					{active, false}
				       ]),
    io:format("Server Start by: ~p~n", [ServerSock]),
    spawn(?MODULE, pre_connect, [ServerSock, Handler]).
pre_connect(ServerSock, Handler) ->
    case ssl:transport_accept(ServerSock) of
	{ok, TransportSock} ->
	    spawn(?MODULE, pre_connect, [ServerSock, Handler]),
	    case ssl:handshake(TransportSock) of
		{ok, ClientSock} ->
		    do_recv(ClientSock, Handler, []);
		{error, closed} ->
		    ok;
		{error, Msg} ->
		    io:format("Handshake Error: ~p~n", [Msg])
	    end;
	{error, Msg} ->
	    io:format("Transport Error: ~p~n", [Msg])
    end.
header_end("\n\r\n\r" ++ _) ->
    true;
header_end(_) ->
    false.
split_recv(ClientSock, Buff, Length) ->
    case length(Buff) == Length of
	true ->
	    {ok, Buff};
	false ->
	    case ssl:recv(ClientSock, 0) of
		{ok, Data} ->
		    split_recv(ClientSock, Buff ++ Data, Length);
		{error, Msg} ->
		    {error, Msg}
	    end
    end.
do_response(ClientSock, Handler, Ctx) ->
    io:format("do_resp: ~p~n", [Ctx]),
    Response = stream_format(Handler(Ctx)),
    io:format("do_resp: ~p~n", [Response]),
    case ssl:send(ClientSock, Response) of
	ok ->
	    %% io:format("send.~n"),
	    ok = ssl:close(ClientSock);
	{error, _} ->
	    ok = ssl:close(ClientSock)
    end.
do_recv(ClientSock, Handler, Buff) ->
    %% io:format("~p~n",[ClientSock]),
    case ssl:recv(ClientSock, 1) of
	{ok, Data} ->
	    %% io:format("ok: ~p~n", [Buff ++ Data]),
	    case header_end( lists:reverse(Buff ++ Data) ) of
		true ->
		    case parse(Buff ++ Data) of
			{ok, Header} ->
			    #{uri:= Uri, method:= Method, info:= Info, version:= Version} = Header,
			    case orddict:take("Content-Length", Info) of
				{Length, _} ->
				    io:format("Length: ~p~n", [Length]),
				    %% too length need translate to stream
				    %% Tail = split_recv(ClientSock, [], list_to_integer(Length)),
				    Ctx = #{uri => Uri, method => Method,
					    info => Info, version => Version,
					    data => [] },
				    do_response(ClientSock, Handler, Ctx);
				error -> %% no body
				    Ctx = #{uri => Uri, method => Method,
					    info => Info, version => Version,
					    data => [] },
				    do_response(ClientSock, Handler, Ctx)
			    end;
			{error, _} ->
			    %% server error parse
			    ok = ssl:close(ClientSock)
		    end;
		false -> 
		    %% http header not end, continue recv
		    do_recv(ClientSock, Handler, Buff ++ Data)
	    end;
	{error, Msg} ->
	    io:format("do_recv: ~p~n", [Msg])
    end.

%% datatype msg = ok | error
%% parse : string -> msg * map
%% map is dict
%% InfoTable : orddict
%% maybe parse have throw match error
parse(String) when is_list(String) ->
    [Header|[Info]] = string:split(String, "\r\n"),
    [Method|[Uri|[Version]]] = string:tokens(Header, " "),
    InfoTable = orddict:from_list([list_to_tuple(string:split(X, ": ")) || X <- string:tokens(Info, "\r\n")]),
    {ok,
     #{ 
	method => Method,
	uri => Uri,
	version => Version,
	info => InfoTable
      }
    };
parse(_) ->
    {error, #{}}.

stream_format([Head|Tail]) -> Head ++ "\r\n" ++ stream_format(Tail);
stream_format([]) -> [].
