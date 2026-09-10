use v6;
use Test;

plan 8;

# `.map` returns a Seq regardless of which dispatch path runs it. An `@`-variable
# receiver takes the rw-writeback path (mutations to `$_` write back to the
# source), which used to hand back a List whenever the block body was too complex
# for the native fast path.
my @l = <a b>;

is (@l.map({ (1,2).Seq })).WHAT.^name, 'Seq', 'simple block body';
is (@l.map({ (1,2).map({$_}) })).WHAT.^name, 'Seq', 'body with a nested .map';
is (@l.map({ (1,2).grep({1}) })).WHAT.^name, 'Seq', 'body with a nested .grep';
is (@l.map({ my $q = (1,2).map({$_}); $q })).WHAT.^name, 'Seq', 'body with a declaration';
is (@l.map(-> $x { (1,2).map(-> $z { $z }) })).WHAT.^name, 'Seq', 'pointy block body';
is (@l.map({ 7 })).WHAT.^name, 'Seq', 'constant body';

# The rw writeback that this path exists for still works. `map` is lazy in
# Rakudo, so force the Seq before checking the source (mutsu maps eagerly, which
# is why it writes back straight away).
my @n = 1, 2, 3;
my $mapped = @n.map({ $_++; (1,2).map({$_}) });
is $mapped.WHAT.^name, 'Seq', 'a $_-mutating block still returns a Seq';
$mapped.eager;
is-deeply @n, [2, 3, 4], '$_ mutation still writes back to the source array';
