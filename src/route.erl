%% -*- coding: utf-8 -*-
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
    case file:read_file(?WEBROOT ++ "/index.html") of 
	{ok, Binary} -> 
	    Data = binary_to_list(Binary),
	    Length = integer_to_list(length(Data)),
	    ["HTTP/1.1 200 OK",
	     "Strict-Transport-Security: max-age=31536000; includeSubDomains",
	     "Content-length: " ++ Length,
	     "Content-type: text/html",
	     "",
	     Data
	    ];
	{error, Msg} ->
	    io:format("get /index.html error: ~p~n", [Msg]),
	    ["HTTP/1.1 404 Not Found",
	     "Strict-Transport-Security: max-age=31536000; includeSubDomains",
	     ""]
    end;
handler(#{uri := Uri, method := Method}) when Method == "GET" ->
    case file:read_file(?WEBROOT ++ Uri) of
	{ok, Binary} ->
	    Data = binary_to_list(Binary),
	    Length = integer_to_list(length(Data)),
	    Type =  guess_type(Uri),
	    io:format(Type ++ "~n"),
	    ["HTTP/1.1 200 OK",
	     "Strict-Transport-Security: max-age=31536000; includeSubDomains",
	     "Content-length: " ++ Length,
	     "Content-type: " ++ Type,
	     "",
	     Data
	    ];
	{error, _} ->
	    ["HTTP/1.1 404 Not Found",
	     "Strict-Transport-Security: max-age=31536000; includeSubDomains",
	     ""]
    end;
%% require two steps , 
%% step one, create hash file directory
handler(#{uri := Uri, method := Method, 
	  data := Data }) when (Method == "POST") and (Uri == "/access") ->
    Bytes = orddict:from_list([list_to_tuple(string:split(X, ": ")) || X <- string:tokens(Data, "\n")]),
    {Hash, _} = orddict:take("hash", Bytes),
    ok = file:make_dir(?FILEROOT ++ Hash),
    ok = file:write_file(?FILEROOT ++ Hash ++ "/config.ini", Data),
    Resp = "[{\"status\": \"ok\"}]",
    io:format("~p ~n", [Resp]),
    ["HTTP/1.1 200 OK",
     "Strict-Transport-Security: max-age=31536000; includeSubDomains",
     "Content-length: " ++ integer_to_list(length(Resp)),
     "Content-type: json/application",
     "",
     Resp
    ];
%% step two, upload file
handler(#{uri := Uri, method := Method,
	  info:= Info, data := Data }) when (Method == "POST") and (Uri == "/upload") ->
    %% io:format("info: ~n~p~n", [Info]),
    case orddict:take("Hash", Info) of
	{Filename, TailInfo} ->
	    {Index, _} = orddict:take("Package", TailInfo),
	    io:format("name: ~p ~p ~p ~n", [http_uri:decode(Filename), Index, length(Data)]),
	    case file:write_file(?FILEROOT ++ Filename ++ "/" ++ Index, Data) of
		ok ->
		    Resp = "[{\"status\": \"ok\"," ++ "\"index\": \"" ++ Index ++ "\""++ "}]",
		    ["HTTP/1.1 200 OK",
		     "Strict-Transport-Security: max-age=31536000; includeSubDomains",
		     "Content-length: " ++ integer_to_list(length(Resp)),
		     "Content-type: json/application",
		     "",
		     Resp
		    ];
		{error, _} -> 
		    Resp = "[{\"status\": \"error\"}]",
		    ["HTTP/1.1 200 OK",
		     "Strict-Transport-Security: max-age=31536000; includeSubDomains",
		     "Content-length: " ++ integer_to_list(length(Resp)),
		     "Content-type: json/application",
		     "",
		     Resp
		    ]
	    end;
	error ->
	    Resp = "[{\"status\": \"fail\"}]",
	    ["HTTP/1.1 200 OK",
	     "Strict-Transport-Security: max-age=31536000; includeSubDomains",
	     "Content-length: " ++ integer_to_list(length(Resp)),
	     "Content-type: json/application",
	     "",
	     Resp
	    ]
    end;
handler(_) ->
    ["HTTP/1.1 404 Not Found",
     "Strict-Transport-Security: max-age=31536000; includeSubDomains",
     ""].
