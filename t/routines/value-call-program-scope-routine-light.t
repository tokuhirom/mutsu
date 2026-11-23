use Test;

# A named routine declared at program scope, called through its code object
# (`.map(&f)`, `.first(&f)`, `my &g = &f; g(...)`), binds on the positional
# light path like a direct call (#18083). It must keep every answer of the
# closure path: free variables read live, a binding failure stays
# X::TypeCheck::Binding, and the shapes the light path does not take (a nested
# routine, named arguments, `is rw`, optionals, `state`) still work.
plan 15;

my $k = 10;
sub add-k(Int $x) { $x + $k }
is (1..3).map(&add-k).join(','), '11,12,13', 'a free variable of the routine';
$k = 20;
is (1..3).map(&add-k).join(','), '21,22,23', '...read live on every call';

sub typed(Str:D $s --> Bool:D) { $s eq 'b' }
is <a b c>.first(&typed), 'b', '.first(&f)';
my &t = &typed;
throws-like { t(42) }, X::TypeCheck::Binding::Parameter, 'a value call keeps the Binding exception';

sub outer() { my $m = 5; sub inner($x) { $x * $m }; (1..3).map(&inner).join(',') }
is outer(), '5,10,15', 'a nested routine reads its declaring frame';

sub two($a, $b) { "$a-$b" }
is (1..4).map(&two).join(','), '1-2,3-4', 'a two-parameter routine through .map';

sub named($x, :$y = 'd') { "$x$y" }
my &n = &named;
is n(2, y => 'q'), '2q', 'a named argument through a value call';

sub cnt($x) { state $c = 0; ++$c }
is (1..3).map(&cnt).join(','), '1,2,3', 'a routine with state';

sub dyn($x) { $*D ~ $x }
my $*D = 'D';
is (1..2).map(&dyn).join(','), 'D1,D2', 'a dynamic variable of the caller';

sub rw($x is rw) { $x++ }
my $v = 1;
my &r = &rw;
r($v);
is $v, 2, 'an is rw parameter through a value call';

sub opt($x, $y?) { $y // 'none' }
is (1..2).map(&opt).join(','), '2', 'an optional parameter takes the second element';

sub sorter($a, $b) { $b <=> $a }
is (3, 1, 2).sort(&sorter).join(','), '3,2,1', 'sort with a named comparator';

my @called;
sub record($x) { @called.push($x); $x }
(1..3).grep(&record);
is @called.join(','), '1,2,3', 'a routine writing a program-scope array';

{
    my $m = 3;
    sub in-block(Int $x) { $x * $m }
    is (1..3).map(&in-block).join(','), '3,6,9', 'a routine declared in a block reads that block';
    sub fact(Int $n) { $n <= 1 ?? 1 !! $n * fact($n - 1) }
    is (1..4).map(&fact).join(','), '1,2,6,24', 'a block-declared routine recursing by name';
}
