-module(htp_util).
-export([version_to_binary/1, to_lower/1, to_upper/1]).
-export([parse_url/1, parse_status/1]).

version_to_binary({1, 0}) -> <<"HTTP/1.0">>;
version_to_binary({1, 1}) -> <<"HTTP/1.1">>.

to_lower(B) ->
	<< << (char_to_lower(C)) >> || << C >> <= B >>.

to_upper(B) ->
	<< << (char_to_upper(C)) >> || << C >> <= B >>.

parse_url(<< "https://", Rest/binary >>) ->
	parse_url(Rest, ssl);
parse_url(<< "http://", Rest/binary >>) ->
	parse_url(Rest, tcp);
parse_url(URL) ->
	parse_url(URL, tcp).

parse_url(URL, Transport) ->
	case binary:split(URL, <<"/">>) of
		[Peer] ->
			{Host, Port} = parse_peer(Peer, Transport),
			{Transport, Peer, Host, Port, <<"/">>};
		[Peer, Path] ->
			{Host, Port} = parse_peer(Peer, Transport),
			{Transport, Peer, Host, Port, [<<"/">>, Path]}
	end.

parse_peer(Peer, Transport) ->
	case binary:split(Peer, <<":">>) of
		[Host] when Transport =:= tcp ->
			{binary_to_list(Host), 80};
		[Host] when Transport =:= ssl ->
			{binary_to_list(Host), 443};
		[Host, Port] ->
			{binary_to_list(Host), list_to_integer(binary_to_list(Port))}
	end.

parse_status(<<"HTTP/", High, ".", Low, " ", S3, S2, S1, " ", StatusStr/binary>>)
when High >= $0, High =< $9, Low >= $0, Low =< $9, S3 >= $0, S3 =< $9, S2 >= $0, 
S2 =< $9, S1 >= $0, S1 =< $9 ->
	Version = {High - $0, Low - $0},
	Status = (S3 - $0) * 100 + (S2 - $0) * 10 + S1 - $0,
	{Status, StatusStr, Version}.

char_to_lower($A) -> $a;
char_to_lower($B) -> $b;
char_to_lower($C) -> $c;
char_to_lower($D) -> $d;
char_to_lower($E) -> $e;
char_to_lower($F) -> $f;
char_to_lower($G) -> $g;
char_to_lower($H) -> $h;
char_to_lower($I) -> $i;
char_to_lower($J) -> $j;
char_to_lower($K) -> $k;
char_to_lower($L) -> $l;
char_to_lower($M) -> $m;
char_to_lower($N) -> $n;
char_to_lower($O) -> $o;
char_to_lower($P) -> $p;
char_to_lower($Q) -> $q;
char_to_lower($R) -> $r;
char_to_lower($S) -> $s;
char_to_lower($T) -> $t;
char_to_lower($U) -> $u;
char_to_lower($V) -> $v;
char_to_lower($W) -> $w;
char_to_lower($X) -> $x;
char_to_lower($Y) -> $y;
char_to_lower($Z) -> $z;
char_to_lower(Ch) -> Ch.

char_to_upper($a) -> $A;
char_to_upper($b) -> $B;
char_to_upper($c) -> $C;
char_to_upper($d) -> $D;
char_to_upper($e) -> $E;
char_to_upper($f) -> $F;
char_to_upper($g) -> $G;
char_to_upper($h) -> $H;
char_to_upper($i) -> $I;
char_to_upper($j) -> $J;
char_to_upper($k) -> $K;
char_to_upper($l) -> $L;
char_to_upper($m) -> $M;
char_to_upper($n) -> $N;
char_to_upper($o) -> $O;
char_to_upper($p) -> $P;
char_to_upper($q) -> $Q;
char_to_upper($r) -> $R;
char_to_upper($s) -> $S;
char_to_upper($t) -> $T;
char_to_upper($u) -> $U;
char_to_upper($v) -> $V;
char_to_upper($w) -> $W;
char_to_upper($x) -> $X;
char_to_upper($y) -> $Y;
char_to_upper($z) -> $Z;
char_to_upper(Ch) -> Ch.
