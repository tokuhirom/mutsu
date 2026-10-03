use Test;

plan 4;

grammar G {
    token TOP { <a> <b> }
    token a { x }
    token b { y }
}

# An actions class whose only method is FALLBACK receives every rule's
# reduction, as Rakudo's find_method hands FALLBACK out for a missing name.
class A {
    has @.seen;
    method FALLBACK($name, $/) { @!seen.push: $name; make ~$/ }
}
my $acts = A.new;
my $m = G.parse("xy", actions => $acts);
is $m.made, 'xy', 'FALLBACK action made the TOP value';
is $m<a>.made, 'x', 'FALLBACK action ran for a subrule';
is $acts.seen.join(','), 'a,b,TOP', 'FALLBACK saw every rule name in reduce order';

# A declared method still wins over FALLBACK.
class B {
    method a($/) { make "A" }
    method FALLBACK($name, $/) { make $name }
}
$m = G.parse("xy", actions => B.new);
is ($m.made, $m<a>.made, $m<b>.made).join(','), 'TOP,A,b', 'declared action preferred over FALLBACK';
