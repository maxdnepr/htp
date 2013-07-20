-module(htp_response).

-author('Maxim Ilyin <maximusdn@gmail.com>').

-export([response/1, response_body/1]).

-include("htp.hrl").

response(Client1) ->
    case status(Client1) of
        {ok, Status, _, Client2} ->
            case header(Client2) of
                {ok, Header, Client3} ->
                    {ok, Status, Header, Client3};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

response_body(#client{status = response_body} = Client) ->
	response_body_loop(Client, <<>>).

response_body_loop(Client, Acc) ->
	case stream_body(Client) of
		{ok, Data, Client2} ->
			response_body_loop(Client2, <<Acc/binary, Data/binary>>);
		{done, Client2} ->
			{ok, Acc, Client2};
		{error, Reason} ->
			{error, Reason}
	end.

header(#client{status = response} = Client) ->
	header(Client, []).

header(Client, Acc) ->
	case stream_header(Client) of
		{ok, Name, Value, Client2} ->
			header(Client2, [{Name, Value}|Acc]);
		{done, Client2} ->
			{ok, Acc, Client2};
		{error, Reason} ->
			{error, Reason}
	end.
    
stream_header(#client{status=response,buffer=Buffer,response_body=RespBody}=Client) ->
	case binary:split(Buffer, <<"\r\n">>) of
		[<<>>, Rest] ->
			Client2 = case RespBody of
				undefined -> Client#client{status=request};
				0 -> Client#client{status=request};
				_ -> Client#client{status=response_body}
			end,
			{done, Client2#client{buffer=Rest}};
		[Line, Rest] ->
			[Name, Value] = binary:split(Line, <<": ">>),
			Name2 = htp_util:to_lower(Name),
			Client2 = case Name2 of
				<<"content-length">> ->
					Length = list_to_integer(binary_to_list(Value)),
					if Length >= 0 -> ok end,
					Client#client{response_body=Length};
				_ ->
					Client
			end,
		    {ok, Name2, Value, Client2#client{buffer=Rest}};
		_ ->
			case recive(Client) of
				{ok, Data} ->
					Buffer2 = << Buffer/binary, Data/binary >>,
					stream_header(Client#client{buffer=Buffer2});
				{error, Reason} ->
					{error, Reason}
			end
	end.

stream_body(#client{status=response_body, response_body = RespBody} = Client)
when RespBody =:= undefined; RespBody =:= 0 ->
	{done, Client#client{status = request, response_body = undefined}};
stream_body(#client{status = response_body, buffer = Buffer,
response_body = Length} = Client) when is_integer(Length) ->
	case byte_size(Buffer) of
		0 ->
			case recive(Client) of
				{ok, Body} when byte_size(Body) =< Length ->
					Length2 = Length - byte_size(Body),
					{ok, Body, Client#client{response_body=Length2}};
				{ok, Data} ->
					<<Body:Length/binary, Rest/binary>> = Data,
					{ok, Body, Client#client{buffer=Rest, response_body=undefined}};
				{error, Reason} ->
					{error, Reason}
			end;
		N when N =< Length ->
			Length2 = Length - N,
			{ok, Buffer, Client#client{buffer = <<>>, response_body = Length2}};
		_ ->
			<<Body:Length/binary, Rest/binary>> = Buffer,
			{ok, Body, Client#client{buffer = Rest, response_body = undefined}}
	end.

status(#client{buffer = Buffer} = Client) ->
	case binary:split(Buffer, <<"\r\n">>) of
		[Line, Rest] ->
			{Status, StatusStr, Version} = htp_util:parse_status(Line),
            {ok, Status, StatusStr, Client#client{status = response, buffer = Rest, 
            version=Version}};
		_ ->
			case recive(Client) of
				{ok, Data} ->   
					Buffer2 = <<Buffer/binary, Data/binary>>,
					status(Client#client{buffer = Buffer2});
				{error, Reason} ->
					{error, Reason}
			end
	end.

recive(Client) ->
    #client{htp_module = HtpModule,
            socket = Socket,
            response_timeout = Timeout} = Client,
    HtpModule:recive(Socket, 0, Timeout).
