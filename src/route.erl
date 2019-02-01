-module(route).
-behaviour(define).
-spec handler(Data :: map()) -> list().
-export([handler/1]).
-include("webconfig.hrl"). %% include() can import macro, import() ???
-import(light_func, [guess_type/1]).

%% {ok, Binary} = file:read_file("filename").
%% Len = length(binary_to_list(Binary)).
%% Erlang `Guard expression` can't use user function, but can using `pattern match` to substitution it
handler(#{uri := Uri, method := Method}) when (Uri == "/") and (Method == "GET") ->
    {ok, Binary} = file:read_file(?WEBROOT ++ "/index.html"),
    Data = binary_to_list(Binary),
    Length = integer_to_list(length(Data)),
    ["HTTP/1.1 200 OK",
     "Content-length: " ++ Length,
     "Content-type: text/html",
     "",
     Data
     ];
handler(#{uri := Uri, method := Method}) when Method == "GET" ->
    case file:read_file(?WEBROOT ++ Uri) of
	{ok, Binary} ->
	    Data = binary_to_list(Binary),
	    Length = integer_to_list(length(Data)),
	    Type =  guess_type(Uri),
	    io:format(Type ++ "~n"),
	    ["HTTP/1.1 200 OK",
	     "Content-length: " ++ Length,
	     "Content-type: " ++ Type,
	     "",
	     Data
	    ];
	{error, _} ->
	    ["HTTP/1.1 404 Not Found",
	     ""]
    end;
handler(_) ->
    ["HTTP/1.1 404 Not Found",
     ""].
