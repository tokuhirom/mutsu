use Test;

# #10960: a frame that declares a named sub env-syncs only the lexicals the
# sub's body reads by name, not every local. Each case below declares the
# outer lexical, then a sub reading it, then re-stores it in the same
# frame: the sub must see the latest store.

plan 11;

my $base = 100;
sub plain-read { $base + 1 }
$base = 200;
is plain-read(), 201, 'named sub reads a re-stored outer scalar';

my $h = {};
sub closure-mutate { -> { $h<a> = 1 }(); $h<a> }
$h = {};
is closure-mutate(), 1, 'container mutation in a closure nested in the sub';

my @a = 1, 2;
sub arr-read { @a.elems }
@a = 1, 2, 3;
is arr-read(), 3, 'named sub reads a re-stored outer array';

my $q = 5;
sub param-default($x = $q) { $x }
$q = 6;
is param-default(), 6, 'parameter default reads the outer scalar';

my $w = 1;
sub writer { $w = $w + 10 }
writer(); writer();
is $w, 21, 'named sub writes accumulate into the outer scalar';

my $n = 3;
sub nested-sub { my sub inner { $n * 2 }; inner() }
$n = 4;
is nested-sub(), 8, 'sub nested in the sub reads the outer scalar';

my $acc = '';
sub map-acc { (1..3).map({ $acc ~= $_ }).eager; $acc }
$acc = 'X';
is map-acc(), 'X123', 'closure inside the sub appends to the outer scalar';

my $o = 1;
sub subst-repl { my $t = 'a'; $t ~~ s/a/$o/; $t }
$o = 9;
is subst-repl(), '9', 'substitution replacement in the sub reads the outer scalar';

my $s = 'abc';
sub regex-interp { 'xabcx' ~~ / $s / ?? 'm' !! 'n' }
$s = 'zz';
is regex-interp(), 'n', 'interpolated regex in the sub reads the outer scalar';

my $mid = 'old';
multi sub mm(Int $x) { "$mid int" }
multi sub mm(Str $x) { "$mid str" }
$mid = 'new';
is mm(1) ~ ',' ~ mm('a'), 'new int,new str', 'multi candidates read the outer scalar';

# A loop local the sub never names stays slot-only; the result is unchanged.
my $t = 0;
my int $i = 0;
sub one() { 1 }
while $i < 1000 { $t = $t + one(); $i = $i + 1 }
is $t, 1000, 'hot loop in a frame that declares a sub';
