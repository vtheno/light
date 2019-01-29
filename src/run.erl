-module(run).
-export([run/0]).

run() ->
    application:start(light).

%% erl -s run run -detached
%% or other remote set: -name remote@r_ip -setcookie private_key
%% werl -setcookie private_key -name locale@l_ip ; and ^G to "r 'remote@r_ip'" and "c"
