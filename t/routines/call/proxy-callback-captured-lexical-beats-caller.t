use Test;
plan 6;

sub mk is rw { my $v = 5; Proxy.new(FETCH => -> $ { $v }, STORE => -> $, $n { $v = $n }) }
my $p := mk();

sub reader($px) { my $v = 'caller'; my $r = $px; ($r, $v) }
is reader($p)[0], 5, 'FETCH reads its own captured $v, not the reader caller\'s';
is reader($p)[1], 'caller', 'the caller\'s $v is untouched';

$p = 7;
is $p, 7, 'STORE then FETCH round-trip through the captured lexical';

sub setter($px is rw) { my $v = 'caller'; $px = 9; $v }
is setter($p), 'caller', 'STORE does not write the caller\'s same-named $v';
is $p, 9, 'the captured $v saw the store';

my $n = 1;
my $pr = start { my $n = 2; $n };
is await($pr), 2, 'a start block keeps its own $n';
