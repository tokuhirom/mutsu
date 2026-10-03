use Test;

# A loop's sub-signature binds like a routine's: `*%rest` collects the named
# part the named parameters did not take, `*@rest` the remaining positionals,
# a named default stands in for an absent key, and `!` makes one required.
# From PDF::Grammar's `for @tests -> % ( :$rule!, :$input, *%expected )`.

plan 9;

my @seen;
for ({ rule => 'r', input => 'i', ast => 1, extra => 2 },) -> % ( :$rule!, :$input, *%expected ) {
    @seen.push: $rule, $input, %expected.keys.sort.join(',');
}
is-deeply @seen, ['r', 'i', 'ast,extra'], '*%rest takes the keys the named params left';

my @rest;
for ([1, 2, 3],) -> ($first, *@others) { @rest = $first, @others }
is-deeply @rest, [1, [2, 3]], '*@rest takes every remaining positional';

my @defaults;
for ({ test => 'a' }, { test => 'b', rule => 'body' }) -> % ( :$test!, :$rule = 'TOP', *%rest ) {
    @defaults.push: "$test:$rule:{%rest.elems}";
}
is-deeply @defaults, ['a:TOP:0', 'b:body:0'], 'a named default applies only when the key is absent';

throws-like { for ({ a => 1 },) -> % ( :$a, :$rule! ) { } },
    Exception, message => /'rule'/, 'a required named sub-parameter must be passed';

my @pairs;
for (a => 1, b => 2) -> (:$key, :$value) { @pairs.push: "$key=$value" }
is-deeply @pairs, ['a=1', 'b=2'], 'Pair destructuring by key/value still works';

my %empty-rest;
for ({ a => 1 },) -> % ( :$a, *%r ) { %empty-rest = %r }
is %empty-rest.elems, 0, 'an empty *%rest when every key was taken';

# The same signatures on a routine agree with the loop form.
sub f(% ( :$rule!, :$input, *%expected )) { "$rule $input {%expected.keys.sort}" }
is f({ rule => 'r', input => 'i', ast => 1 }), 'r i ast', 'routine form agrees';

my $n = 0;
for ({ x => 1 }, { x => 2 }) -> % ( :$x, :$y = $x * 10 ) { $n += $y }
is $n, 30, 'a default may refer to an earlier parameter';

my @w;
for ([1],) -> ($a, *@b) { @w = @b }
is @w.elems, 0, '*@rest is empty when nothing remains';
