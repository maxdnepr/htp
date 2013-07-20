-module(htp_tcp).
-export([connect/4, close/1]).
-export([send/2, recive/3]).

connect(Host, Port, Opts, Timeout) ->
    ListOpts = Opts ++ [binary, {active, false}, {packet, raw}],
    Result = gen_tcp:connect(Host, Port, ListOpts, Timeout),
    case Result of
        {ok, Socket} ->
            {ok, Socket};
        {error, Reason} ->
            {error, Reason}
    end.

close(Socket) ->
    ok = gen_tcp:close(Socket).

send(Socket, Data) ->
    ssl:send(Socket, Data).

recive(Socket, Length, Timeout) ->
    gen_tcp:recv(Socket, Length, Timeout).
