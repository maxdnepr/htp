-module(htp).

-author('Maxim Ilyin <maximusdn@gmail.com>').

-export([start/0, stop/0]).
-export([connect/3, connect/4, close/1]).
-export([request/3, request/4, request/5]).
-export([response_body/1]).

-include("htp.hrl").

start() ->
    start(?MODULE).

stop() ->
    application:stop(?MODULE).

start(AppName) ->
    F = fun({App, _, _}) -> App end,
    RunningApps = lists:map(F, application:which_applications()),
    ok = load(AppName),
    {ok, Dependencies} = application:get_key(AppName, applications),
    [begin
        ok = start(A)
    end || A <- Dependencies, not lists:member(A, RunningApps)],
    ok = application:start(AppName).

load(AppName) ->
    F = fun({App, _, _}) -> App end,
    LoadedApps = lists:map(F, application:loaded_applications()),
    case lists:member(AppName, LoadedApps) of
        true ->
            ok;
        false ->
            ok = application:load(AppName)
    end.

connect(Transport, Host, Port) when is_atom(Transport), is_list(Host), is_integer(Port) ->
    htp_client:connect(Transport, Host, Port).

connect(Transport, Host, Port, Opts) when is_atom(Transport), is_list(Host), 
is_integer(Port), is_list(Opts) ->
    htp_client:connect(Transport, Host, Port, Opts).

close(Client) when is_record(Client, client) ->
    htp_client:close(Client).

request(Method, Url, Client) when is_binary(Method), is_binary(Url), 
is_record(Client, client) ->
    htp_client:request(Method, Url, Client).

request(Method, Url, Headers, Client) when is_binary(Method), is_binary(Url), 
is_list(Headers), is_record(Client, client) ->
    htp_client:request(Method, Url, Headers, Client).

request(Method, Url, Headers, Body, Client) when is_binary(Method), is_binary(Url), 
is_list(Headers), is_binary(Body), is_record(Client, client) ->
    htp_client:request(Method, Url, Headers, Body, Client).

response_body(Client) when is_record(Client, client) ->
    htp_response:response_body(Client).
