-module(route).
-import(lightWeb, [stream_format/1]).
-behaviour(lightDef).
-spec handler(Data :: map()) -> list().
-export([handler/1]).

handler(#{uri := Uri, method := Method}) when (Uri == "/") and (Method == "GET") ->
    %% io:format("~s ~n",[Uri]),
    lightWeb:stream_format(["HTTP/1.1 200 OK",
			    "Content-length: 5",
			    "Content-type: text/plain",
			    "",
			    "index"]);
handler(_) ->
    lightWeb:stream_format(["HTTP/1.1 404 Page Not Found",
			    ""]).
