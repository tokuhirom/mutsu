use Test;

# `BIND-KEY` / `BIND-POS` are rows that bind an element to the caller's
# variable (ADR-11276 §9.41): the variable and the element share one cell.

plan 14;

my %h;
my $x = 1;
%h.BIND-KEY('k', $x);
$x = 2;
is %h<k>, 2, 'BIND-KEY: a write to the variable reaches the element';
%h<k> = 3;
is $x, 3, 'BIND-KEY: a write to the element reaches the variable';

my @a = 1, 2;
my $y = 10;
@a.BIND-POS(1, $y);
$y = 11;
is @a[1], 11, 'BIND-POS: a write to the variable reaches the element';
@a[1] = 12;
is $y, 12, 'BIND-POS: a write to the element reaches the variable';

my $z = 5;
@a.BIND-POS(4, $z);
is @a.elems, 5, 'BIND-POS past the end grows the array';
ok !@a[3].defined, 'the gap stays a hole';
is @a[4], 5, 'the bound element reads the variable';

my @alias := @a;
my $w = 7;
@a.BIND-POS(0, $w);
$w = 8;
is @alias[0], 8, 'the bind is seen by another holder of the array';

my %typed{Any};
my $kv = 'v';
%typed.BIND-KEY(42, $kv);
$kv = 'w';
is %typed{42}, 'w', 'BIND-KEY on an object hash';

throws-like { my $s = Set.new(<a>); $s.BIND-KEY('a', $x) }, X::Bind, 'Set refuses BIND-KEY';
throws-like { my $b = BagHash.new(<a>); $b.BIND-KEY('a', $x) }, X::Bind, 'BagHash refuses BIND-KEY';
throws-like { my $m = Mix.new(<a>); $m.BIND-KEY('a', $x) }, X::Bind, 'Mix refuses BIND-KEY';

my %v;
%v<q> := $x;
$x = 99;
is %v<q>, 99, 'the := form binds the element';

my @n;
@n[2] := $z;
$z = 6;
is @n[2], 6, 'the := form binds the array element';
