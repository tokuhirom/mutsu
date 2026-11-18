use Test;

# A `with` / `without` whose body never names `$_` reads its topic as a plain
# value instead of tagging the source container for aliasing (#15737). The
# forms whose body does use `$_` keep the alias and its write-back. Each
# shape runs hot.

plan 12;

# Untagged: the body only returns / dies / reads other variables.
sub memo(@m) {
    my $known = @m[0];
    return Int without $known[1];
    $known[0]
}
my @m = [3, 4],;
my $r;
$r = memo(@m) for ^50;
is $r, 3, 'return-without over an element read';
my @n = [3, Int],;
is memo(@n).raku, 'Int', '... and the without branch when undefined';

sub die-with($x) { my $v = $x; die "got $x" with $v; 'none' }
is die-with(Nil), 'none', 'die-with over an undefined variable';
throws-like { die-with(5) }, X::AdHoc, 'die-with over a defined variable';

sub return-with(%h) { return %h<k> + 1 with %h<k>; 0 }
my $t = 0;
$t += return-with(%(k => $_)) for ^50;
is $t, (1..50).sum, 'return-with reading the element again, not via $_';
is return-with(%()), 0, '... and the fall-through';

sub next-without(@a) { my $s = 0; for @a -> $e { next without $e; $s += $e }; $s }
is next-without([1, Int, 2, Nil, 3]), 6, 'next-without inside a loop';

# Tagged: the body reads or writes $_.
sub with-topic($x) { my $v = $x; my $out = ''; $out = $_ * 2 with $v; $out }
is with-topic(21), 42, 'a with body that reads $_ sees the topic';
sub with-write() { my $v = 1; $_ = 10 with $v; $v }
is with-write(), 10, 'a with body that assigns $_ writes the variable';
sub with-elem-write() { my %h = k => 1; $_++ with %h<k>; %h<k> }
is with-elem-write(), 2, '... and the element';
sub without-block($x) { my $v = $x; without $v { $v = 'filled' }; $v }
is without-block(Nil), 'filled', 'a without block still runs';
sub with-call($x) { my $v = $x; my $s = ''; $s = $_.Str with $v; $s }
is with-call(7), '7', 'a method call on $_';
