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

plan 33;

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

# --- #8357: a sub-signature naming only PART of a named capture rejects ----
#
# A `Pair`'s capture is exactly `\(:key(…), :value(…))`; naming only one of
# the two leaves the other unaccounted, and rakudo rejects the bind the same
# way it rejects a surplus named argument at an ordinary call. This is the
# BINDER's own check (a plain, non-multi `if`/`given`/`with`/`for` never
# consults the multi-dispatch matcher that already had this rule).
{
    my $err;
    try { if (a => 1) -> (:key($k)) { } };
    $err = $!;
    is $err.message, "Unexpected named argument 'value' passed in sub-signature",
        'a sub-signature naming only "key" rejects the unaccounted "value"';
}

# The same rule for an all-named Hash/Map destructure: every entry is a named
# capture part with no positional part at all.
{
    my $err;
    try { if {op => 'add', path => '/x'} -> (:$op) { } };
    $err = $!;
    is $err.message, "Unexpected named argument 'path' passed in sub-signature",
        'a Hash destructure naming only one key rejects the unaccounted rest';
}

# A bare `|` capture (unlike a typed `*@rest`, which does NOT exempt a named
# surplus) swallows everything, named arguments included, and must NOT
# regress into rejecting it.
if (a => 1) -> (:key($k), |) {
    is $k, 'a', 'a `|` capture after a partial named destructure still binds';
}
if {op => 'add', path => '/x'} -> (:$op, |) {
    is $op, 'add', 'a `|` capture after a partial Hash destructure still binds';
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

# --- `if` / `elsif` / `unless` / `else`, the same gap (#8340) ---------------
#
# Two halves. The parameter PARSER read `-> (...)` as the signature's own
# parentheses and handed the inside to the ordinary parameter-list parser, so
# `-> ($a, $b)` became two positionals; it now records one destructuring
# parameter, as `for` / `given` / a bare `-> (...)` lambda already did. And the
# clause was CALLED with the condition slipped (`|$tmp`), so a list condition
# bound several parameters; rakudo passes the condition as one argument and lets
# the signature decide.

if (1, 2) -> ($a, $b) { is "$a $b", '1 2', 'if destructures a list condition' }

{
    my $ran = False;
    unless 0 -> $c { $ran = $c }
    is $ran, 0, 'unless passes the condition as one argument too';
}

{
    my $ran = False;
    if 0 { } elsif (7, 8) -> ($a, $b) { $ran = "$a $b" }
    is $ran, '7 8', 'elsif destructures too';
}
{
    my $ran;
    if 0 { } else -> $c { $ran = $c }
    is $ran, 0, '`else -> $c` receives the condition as one argument';
}

# The condition is ONE argument, so a two-parameter signature is an arity
# error -- it is not a two-way bind.
{
    my $err;
    try { EVAL 'if (1, 2) -> $a, $b { }' };
    $err = $!;
    ok $err.defined, 'a list condition does not bind two plain parameters';
}

# The slurpy spellings all follow from that single rule, and are what
# `roast/S04-statements/if.t` pins.
if 1, 2 -> +@a { is-deeply @a, [1, 2], '+@ applies the one-argument rule' }
if 42, 42 -> **@a { is-deeply @a, [(42, 42),], '**@ keeps the list whole' }
if 1, (2, (3, $(4, 5))) -> *@a {
    is-deeply @a, [1, 2, 3, $(4, 5)], '*@ flattens the single list argument';
}
