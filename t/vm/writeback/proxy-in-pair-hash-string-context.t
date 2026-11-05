use Test;
plan 10;

my $v = 7;
my @buf;
@buf[0] := Proxy.new(FETCH => method () { $v }, STORE => method ($x) { $v = $x });
@buf[1] := Proxy.new(FETCH => method () { 8 }, STORE => method ($x) { });

my $p = (a => @buf[0]);
is "$p", "a\t7", 'interpolating a Pair holding a Proxy FETCHes it';
is $p.Str, "a\t7", '.Str of the Pair FETCHes it';
is "x" ~ $p, "xa\t7", 'infix ~ FETCHes it';
ok $p eq "a\t7", 'infix eq FETCHes it';

my $m = Map.new: reverse <a b> Z=> @buf.Seq;
is "$m", "a\t7\nb\t8", 'a Map built from Proxy elements interpolates their values';
ok $m eq Map.new(<a>, <7>, <b>, <8>), 'and compares equal to the plain Map';
is $m.Str, "a\t7\nb\t8", '.Str of the Map';

my @z = <a b> Z=> @buf.Seq;
is "@z[]", "a\t7 b\t8", 'a list of Pairs holding Proxies interpolates their values';

my %h = a => @buf[0];
is "{%h}", "a\t7", 'a Hash holding a Proxy value';

$v = 9;
is "$p", "a\t9", 'the FETCH is re-run on every read';
