use Test;

# An ANONYMOUS destructuring parameter may carry a type constraint:
# `-> Pair (:key($k), :value($v))`. Only the named spelling
# (`-> Pair $p (:$key)`) used to parse — the block-parameter parsers keep a
# type constraint only when a sigil follows it, so the anonymous form reached
# no branch at all and the whole block (or `for` header) failed to parse.
# From Config::BINDish and Red, both of which failed to load on it.

plan 9;

my @pairs = (a => 1,);
my @lists = ((1, 2), (3, 4));

for @pairs -> Pair (:key($k), :value($v)) {
    is "$k=$v", 'a=1', 'for -> Pair (:key, :value)';
}

for @pairs -> Pair:D (:key($k), Str() :value($v)) {
    is "$k=$v", 'a=1', 'for -> Pair:D with a coercion type inside';
}

my @sums;
for @lists -> List ($a, $b) { @sums.push: $a + $b }
is-deeply @sums, [3, 7], 'for -> List ($a, $b)';

for ((1, 2),) -> List [$a, $b] {
    is $a + $b, 3, 'for -> List [$a, $b]';
}

my $pair = a => 1;
my $block = -> Pair (:key($k), :value($v)) { "$k$v" };
is $block($pair), 'a1', 'pointy block -> Pair (:key($k), :value($v))';

my $other = b => 2;
my $two = -> Pair (:key($k), :value($)), Pair (:key($j), :value($)) { "$k$j" };
is $two($pair, $other), 'ab', 'two anonymous typed destructures';

# The type constraint is not decoration: it is checked.
throws-like { for ((1, 2),) -> Pair (:key($k), :value($v)) { } },
    X::TypeCheck::Binding,
    'the type constraint on an anonymous destructure is enforced';

# Untyped and named spellings keep working.
my @plain;
for @lists -> ($a, $b) { @plain.push: $a + $b }
is-deeply @plain, [3, 7], 'untyped -> ($a, $b) still parses';

for @pairs -> Pair $p (:key($k), :value($v)) {
    is "$k:{$p.value}", 'a:1', 'named -> Pair $p (:key($k)) still parses';
}
