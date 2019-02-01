-module(light_func).
-export([guess_type/1]).

wrap_guess_type("sj." ++ _) ->
    "application/javascript";
wrap_guess_type("ssc." ++ _) ->
    "text/css";
wrap_guess_type("ffow." ++ _) ->
    "font/woff";
wrap_guess_type("2ffow." ++ _) ->
    "font/woff2";
wrap_guess_type("piz." ++ _) ->
    "application/zip";
wrap_guess_type(_) ->
    "application/octet-stream".
guess_type(Inp) ->
    wrap_guess_type(lists:reverse(Inp)).
