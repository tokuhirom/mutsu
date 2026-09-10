use Test;

# A single-param `for` loop inside an escaping closure must read its OWN
# per-iteration binding, not a same-named outer lexical the closure happens
# to capture. The loop param's binding write happens inside the ForLoop
# opcode exec (not a compiled name-write op), so it gave `compute_free_vars`
# no declaring construct to recognize -- a pure body read of the param name
# was misclassified as a FREE variable and rewritten to `GetUpvalue`, which
# resolves against whatever the closure captured under that name instead of
# the loop's own binding.
#
# Real-world failure: Cro::HTTP::Router::LinkGenerator's `signature-to-sub`
# builds an index counter `my $i = -1; for ...; $i++ ...` then returns a
# closure containing `for @fn-parts -> $i { @result[$i] = ... }` -- every
# call saw $i frozen at the counter's final build-time value instead of each
# iteration's own value (`t/../roast Cro::HTTP suite http-router-named-urls.t`
# "Escaped named param" / "Escaped positional").

plan 3;

sub make() {
    my $i = -1;
    my @parts = 1,;
    for 1..3 { $i++ }
    -> {
        my $seen;
        for @parts -> $i {
            $seen = $i;
        }
        $seen;
    }
}
is make()(), 1, 'loop param reads its own per-iteration binding, not the captured outer counter';

# Same shape but the closure returns every iteration's value, and the outer
# counter keeps mutating after the closure is created -- the loop must not
# ever see the counter's value at all.
sub make2() {
    my $n = -1;
    for 1..5 { $n++ }
    my @items = 10, 20, 30;
    -> {
        my @seen;
        for @items -> $n {
            @seen.push($n);
        }
        @seen;
    }
}
is make2()().join(','), '10,20,30', 'every iteration sees its own value across a run';

# The outer counter itself must be untouched by the loop reusing its name.
sub make3() {
    my $k = 0;
    for 1..3 { $k++ }
    my @xs = 100,;
    my $closure = -> {
        my $seen;
        for @xs -> $k { $seen = $k }
        $seen;
    };
    ($closure(), $k);
}
my ($inner, $outer) = make3();
is "$inner,$outer", '100,3', 'the loop param does not clobber the outer counter it shadows';
