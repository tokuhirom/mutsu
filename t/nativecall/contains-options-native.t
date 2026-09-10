use Test;

# Pin for the ledger §D(b) slice that drains the positioned / case-insensitive forms
# of Str.contains to VM-native dispatch. The bare `contains($needle)` form was already
# native (native_method_1arg); the forms carrying a start position and/or the
# `:i`/`:ignorecase`/`:m`/`:ignoremark` markings push past the arity-keyed native
# dispatch (a Pair or a 3rd arg), so they previously bounced to the interpreter.

plan 22;

# bare needle (already native — sanity)
is "hello".contains("ell"), True, 'bare needle present';
is "hello".contains("xyz"), False, 'bare needle absent';

# case-insensitive (named, no position)
is "hello".contains("L", :i), True, ':i present';
is "hello".contains("L"), False, 'case-sensitive absent';
is "Foo".contains("foo", :ignorecase), True, ':ignorecase present';
is "hello".contains("L", :!i), False, ':!i is case-sensitive';
is "café".contains("É", :i), True, ':i over unicode';

# start position (positional)
is "hello".contains("l", 3), True, 'position hits';
is "hello".contains("l", 4), False, 'position misses';
is "hello".contains("h", 1), False, 'position skips earlier match';
is "hello".contains("", 2), True, 'empty needle in-bounds';
is "hello".contains("", 5), True, 'empty needle at len';
is "hello".contains("l", "3"), True, 'string position coerces';

# position + case-insensitive together
is "hello".contains("L", 2, :i), True, 'position + :i present';
is "hello".contains("L", 4, :i), False, 'position + :i past match';

# junction needle (threaded; result is a Junction, collapse in boolean context)
ok so "hello".contains(any("z", "ell")), 'any-junction needle';
ok so "hello".contains(all("ell", "lo")), 'all-junction needle present';
nok so "hello".contains(all("ell", "zz")), 'all-junction needle absent';

# error semantics preserved (fall through to interpreter)
{
    my $r = "foo".contains("o", -42);
    ok $r ~~ Failure, 'negative position returns a Failure';
    $r.so; # defuse
}
{
    my $r = "foo".contains("o", 99999999999999999999999999999);
    ok $r ~~ Failure, 'huge BigInt position returns a Failure';
    $r.so;
}

# Match invocant still works (interpreter coercion path)
my $m = "foobara".match(/\w+/);
is $m.contains("bar"), True, 'Match invocant contains present';
is $m.contains("zzz"), False, 'Match invocant contains absent';
