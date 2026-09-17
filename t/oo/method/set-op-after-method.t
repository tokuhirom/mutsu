# A `(...)`-delimited set/baggy infix operator immediately after a method call,
# with a space before it (`@a.Set (|) @b.Set`), is an operator — not a
# space-separated call-arg list. mutsu erroneously rejected the space with
# "no space allowed between method name and the left parenthesis".
#
# The same construct with an *implicit* invocant (`.value (elem) $set`, calling
# a no-arg method on the topic `$_`) went through a separate parser code path
# (primary/regex/lit.rs) that lacked the same allowance, so `for @pairs { if
# .value (elem) $rename_set { ... } }` — found in the wild in the
# File::Name::Editor distribution — failed to parse at all.
use Test;

plan 12;

is ((1,2,3).Set (|) (3,4).Set).keys.sort.join(","), '1,2,3,4', 'union (|) after .Set';
is ((1,2,3).Set (&) (2,3,4).Set).keys.sort.join(","), '2,3', 'intersection (&) after .Set';
is ((1,2,3).Set (-) (2).Set).keys.sort.join(","), '1,3', 'difference (-) after .Set';
is ((1,2).Set (^) (2,3).Set).keys.sort.join(","), '1,3', 'symmetric diff (^) after .Set';
ok ((1,2).Set (<=) (1,2,3).Set), 'subset (<=) after .Set';
ok ((1,2,3).Set (>) (1,2).Set), 'strict superset (>) after .Set';
ok (2 (elem) (1,2,3).Set), '(elem) before .Set';
ok ((1,2,3).Set (cont) 2), '(cont) after .Set';
is ((1,2).Bag (+) (2,3).Bag).total, 4, 'baggy union (+) after .Bag';

# A genuine space-before-call-paren is still an error.
ok (try { EVAL 'my @a = 1, 2; @a.map (1)' } === Nil and $!.defined),
    'real "space before call paren" still rejected';

# Same operator, implicit invocant (topic `.method`), inside `for`/`if` — the
# File::Name::Editor shape.
{
    my $rename_set = SetHash.new(<a b c>);
    my @pairs = (a => 1), (x => 2);
    my @seen;
    for @pairs {
        if .value (elem) $rename_set {
            @seen.push(.key);
        }
    }
    is @seen.join(","), '', '.value (elem) $set on topic finds no match';

    my $count_set = SetHash.new(1);
    my @matches;
    for @pairs {
        @matches.push(.key) if .value (elem) $count_set;
    }
    is @matches.join(","), 'a', '.value (elem) $set on topic finds a match';
}
