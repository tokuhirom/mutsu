use Test;

# A code parameter defaulting to a built-in operator (`:&cmp = &[cmp]`) and
# called as `cmp($a, $b)` runs that operator -- and still honours a user
# candidate in scope, junction autothreading and a passed-in replacement.

plan 7;

sub order(:&cmp = &[cmp]) { cmp(3, 2) }
is order(), More, 'the built-in default';

sub add(:&op = &[+]) { op(2, 3) }
is add(), 5, 'another built-in operator';

sub any-of(:&op = &[cmp]) { op(1 | 3, 2) }
is any-of().raku, any(Less, More).raku, 'a junction argument autothreads';

sub strs(:&op = &[cmp]) { op('b', 'a') }
is strs(), More, 'string operands';

is order(cmp => -> $a, $b { 'mine' }), 'mine', 'a passed block replaces the operator';

{
    class P { has $.v }
    multi sub infix:<cmp>(P $a, P $b) { 'overridden' }
    sub inner(:&cmp = &[cmp]) { cmp(P.new(v => 1), P.new(v => 2)) }
    is inner(), 'overridden', 'a user candidate in scope still wins';
}

sub loop-it(:&cmp = &[cmp]) {
    my $more = 0;
    $more++ if cmp($_, 5) === More for ^10;
    $more
}
is loop-it(), 4, 'in a loop';
