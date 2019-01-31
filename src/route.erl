-module(route).
-import(light, [stream_format/1]).
-behaviour(define).
-spec handler(Data :: map()) -> list().
-export([handler/1]).

handler(#{uri := Uri, method := Method}) when (Uri == "/") and (Method == "GET") ->
    %% io:format("~s ~n",[Uri]),
    light:stream_format(["HTTP/1.1 200 OK",
			 "Content-length: 5",
			 "Content-type: text/plain",
			 "",
			 "index"]);
handler(_) ->
    light:stream_format(["HTTP/1.1 404 Not Found",
			 ""]).
