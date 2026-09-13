use Test;

# `given` and `with` accept a DESTRUCTURING pointy parameter, exactly as `for`
# does: `given $obj -> (:@list, |) { ... }` unpacks the topic into the
# sub-signature's lexicals instead of binding one name.
#
# `given` used to drop the sub-signature outright — the topic was bound to the
# parameter's synthetic name and the inner lexicals were never declared, so
# every one of them read empty. `with` kept a copy of the unpack that called a
# method named after the sub-parameter *with its sigil still attached*
# (`$obj.@list`), and knew nothing of hash fallback, `|` captures, renames or
# defaults. Both now use the one lowering `for` already had.
#
# From ASTQuery::Match's `given $m -> ::?CLASS:D (:@list, :%hash, |) { ... }`
# (#7988).

plan 21;

class Node {
    has @.list;
    has %.hash;
}

my $node = Node.new(list => [1, 2, 3], hash => {a => 1});

given $node -> (:@list, :%hash, |) {
    is @list.join(','), '1,2,3', 'given -> (:@list, ...) binds the array attribute';
    is %hash<a>, 1, 'given -> (..., :%hash, |) binds the hash attribute';
}

with $node -> (:@list, :%hash, |) {
    is @list.join(','), '1,2,3', 'with -> (:@list, ...) binds the array attribute';
    is %hash<a>, 1, 'with -> (..., :%hash, |) binds the hash attribute';
}

# Positional destructure.
given (1, 2) -> ($a, $b) {
    is "$a/$b", '1/2', 'given -> ($a, $b) destructures positionally';
}
with (3, 4) -> ($a, $b) {
    is "$a/$b", '3/4', 'with -> ($a, $b) destructures positionally';
}

# A named sub-parameter renamed onto another variable (`:key($k)`) — the copy
# `with` used to carry had no notion of this.
given (a => 1) -> (:key($k), :value($v)) {
    is "$k=$v", 'a=1', 'given -> (:key($k), :value($v)) renames';
}
with (b => 2) -> (:key($k), :value($v)) {
    is "$k=$v", 'b=2', 'with -> (:key($k), :value($v)) renames';
}

# A Hash topic has no accessor method per key, so the unpack falls back to a
# key lookup.
my %conf = host => 'example', port => 80;
given %conf -> (:$host, :$port) {
    is "$host:$port", 'example:80', 'given -> (:$host, :$port) falls back to hash keys';
}
with %conf -> (:$host, :$port) {
    is "$host:$port", 'example:80', 'with -> (:$host, :$port) falls back to hash keys';
}

# A defaulted / optional sub-parameter.
given (7,) -> ($a, $b = 42) {
    is "$a/$b", '7/42', 'given destructure honours a default';
}
with (8,) -> ($a, $b?) {
    nok $b.defined, 'with destructure leaves an optional sub-parameter undefined';
}

# A `|` capture sub-parameter collects the tail.
given (1, 2, 3) -> ($first, |rest) {
    is $first, 1, 'given destructure binds the leading positional';
    is rest.elems, 2, 'given destructure binds the | capture to the tail';
}

# `without` takes the same parameter shape, and runs when the topic is
# undefined — there is nothing to unpack, so every sub-parameter is undefined.
my $nothing;
my $ran = 0;
without $nothing -> (:$x, |) { $ran = 1; nok $x.defined, 'without destructure of an undefined topic' }
is $ran, 1, 'without -> (:$x, |) runs for an undefined topic';

# A COERCION type on a sub-parameter coerces the extracted value. `with` was
# the only one of the three that did this; the shared lowering does it for all
# of them now.
given (1, 2) -> (Str() $a, $b) {
    is $a.^name, 'Str', 'given destructure applies a coercion type';
}
with (a => 1) -> (:key($k), Str() :value($v)) {
    is $v.^name, 'Str', 'with destructure applies a coercion type to a named sub-parameter';
}
for ((3, 4),) -> (Str() $a, $b) {
    is $a.^name, 'Str', 'for destructure applies a coercion type';
}
# A plain nominal constraint is not a coercion and must not convert.
for ((5, 6),) -> (Int $a, $b) {
    is $a.^name, 'Int', 'a plain type constraint on a sub-parameter does not coerce';
}

# The non-destructuring pointy parameter is untouched: it still aliases the
# topic source, so a mutation through it writes back.
my @src = 1, 2;
given @src -> @p { @p.push: 3 }
is @src.join(','), '1,2,3', 'a plain pointy parameter still aliases the topic';
