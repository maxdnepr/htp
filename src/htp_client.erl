-module(htp_client).

-author('Maxim Ilyin <maximusdn@gmail.com>').

-export([connect/3, connect/4, close/1]).
-export([request/3, request/4, request/5]).

-include("htp.hrl").

connect(Transport, Host, Port) ->
    connect(Transport, Host, Port, []).

connect(Transport, Host, Port, Opts) ->
    Client = #client{},
    #client{connect_timeout = ConnectTimeout,
            response_timeout = ResponseTimeout} = Client,
    CT = proplists:get_value(connect_timeout, Opts, ConnectTimeout),
    RT = proplists:get_value(response_timeout, Opts, ResponseTimeout),
    OptsOut = proplists:delete(connect_timeout, Opts),
    OptsFin = proplists:delete(response_timeout, OptsOut),
    NewClient = #client{transport = Transport,
                        connect_timeout = CT,
                        response_timeout = RT, 
                        host = Host, 
                        port = Port,
                        options = OptsFin},
    connect(NewClient).

connect(#client{status = closed, transport = Transport} = Client) ->
    HtpModule = case Transport of
        ssl -> htp_ssl;
        tcp -> htp_tcp
    end,
    NewClient = htp_connect(Client#client{htp_module = HtpModule}),
    connect(NewClient);
connect(#client{status = connected} = Client) ->
    {ok, Client};
connect(#client{status = Error} = _) ->
    {error, Error}.

htp_connect(Client) ->
    #client{htp_module = HtpModule,
           host = Host,
           port = Port,
           options = Opts,
           connect_timeout = ConTimeout} = Client,
    Result = HtpModule:connect(Host, Port, Opts, ConTimeout),
    case Result of
        {ok, Socket} ->
            Client#client{status=connected, socket=Socket};
        {error, Reason} ->
            Client#client{status = Reason}
    end.

close(#client{htp_module = HtpModule, socket = Socket} = _) ->
    HtpModule:close(Socket).

request(Method, Url, Client) ->
    request(Method, Url, [], <<>>, Client).

request(Method, Url, Headers, Client) ->
   request(Method, Url, Headers, <<>>, Client).

request(Method, Url, Headers, Body, #client{status = Status} = Client) ->
    {Transport, FullHost, Host, Port, Path} = htp_util:parse_url(Url),
    {ok, NewClient} = case Status of
        closed -> connect(Transport, Host, Port, Client);
        connected -> {ok, Client}
    end,    
    VersionBin = htp_util:version_to_binary(NewClient#client.http_version),
    Headers2 = [{<<"host">>, FullHost}, {<<"user-agent">>, <<"Cow">>}|Headers],
	Headers3 = case iolist_size(Body) of
		0 -> Headers2;
		Length -> 
            [{<<"content-length">>, integer_to_list(Length)}|Headers2]
	end,
	HeadersData = [[Name,<<": ">>,Value,<<"\r\n">>]||{Name,Value}<-Headers3],
	Data = [Method, <<" ">>, Path, <<" ">>, VersionBin, <<"\r\n">>,
		    HeadersData, <<"\r\n">>, Body],
	Result = raw_request(Data, NewClient),
    case Result of
        {ok, ReqClient} ->
            htp_response:response(ReqClient#client{status = request});
        {error, Reason} ->
            {error, Reason}
    end.

raw_request(Data, #client{htp_module = HtpModule, socket = Socket} = Client) ->
    Result = HtpModule:send(Socket, Data),
    case Result of
        ok ->
            {ok, Client};
        {error, Reason} ->
            {error, Reason}
    end.








     

