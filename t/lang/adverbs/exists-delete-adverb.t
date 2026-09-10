# 6.e for the multi-dimensional block at the end: `@a[0;1;2]:exists:delete`
# only has a candidate there. Everything above it is version-independent.
use v6.e.PREVIEW;
use Test;

# `:exists:delete` deletes the elements and answers whether each one had
# existed. Only the reverse order (`:delete:exists`) used to work: once
# `:exists` had been folded into an Exists node, a trailing `:delete` was
# parsed as a `.DELETE-KEY` method call on the *answer* -- a Bool for a single
# index, a List for a slice -- and died with "No such method 'DELETE-KEY'".
#
# The two orders are the same operation, so every case below is checked in
# both.

plan 48;

# --- Array target ---------------------------------------------------------
{
    my @a = 1, 2, 3;
    is-deeply (@a[0]:exists:delete), True, 'a single index answers a bare Bool';
    is-deeply @a, [Any, 2, 3], 'and the slot became a hole';
}
{
    my @a = 1, 2, 3;
    is-deeply (@a[0]:delete:exists), True, ':delete:exists agrees';
    is-deeply @a, [Any, 2, 3], 'and deletes the same slot';
}
{
    my @a = 1, 2, 3;
    is-deeply (@a[0,1]:exists:delete), (True, True), 'a slice answers one Bool per index';
    is-deeply @a, [Any, Any, 3], 'and deletes every named slot';
}
{
    my @a = 1, 2, 3;
    is-deeply (@a[0..1]:exists:delete), (True, True), 'a Range index is a slice here too';
    is-deeply @a, [Any, Any, 3], 'and deletes the slots it names';
}
{
    my @a = 1, 2, 3;
    is-deeply (@a[1,5]:exists:delete), (True, False),
        'a missing index answers False and deletes nothing';
    is-deeply @a, [1, Any, 3], 'the present one is still deleted';
}

# A zen slice names every element, so it deletes the lot -- the same set the
# whatever slice does.
{
    my @a = 1, 2, 3;
    is-deeply (@a[]:exists:delete), (True, True, True), 'a zen slice answers per element';
    is-deeply @a, [], 'and empties the array';
}
{
    my @a = 1, 2, 3;
    is-deeply (@a[*]:exists:delete), (True, True, True), 'a whatever slice does the same';
    is-deeply @a, [], 'and empties the array';
}
{
    my @a = 1, 2, 3;
    is-deeply (@a[]:delete:exists), (True, True, True), 'the zen slice in the other order';
    is-deeply @a, [], 'and empties the array';
}

# --- Hash target ----------------------------------------------------------
{
    my %h = a => 1, b => 2;
    is-deeply (%h<a>:exists:delete), True, 'a single key answers a bare Bool';
    is-deeply %h, {b => 2}, 'and the key is gone';
}
{
    my %h = a => 1, b => 2, c => 3;
    is-deeply (%h<a b>:exists:delete), (True, True), 'a key slice answers one Bool per key';
    is-deeply %h, {c => 3}, 'and every named key is gone';
}
{
    my %h = a => 1, b => 2;
    is-deeply (%h<a zz>:exists:delete), (True, False), 'a missing key answers False';
    is-deeply %h, {b => 2}, 'and only the present key is removed';
}
{
    my %h = a => 1, b => 2;
    is-deeply (%h{}:exists:delete), (True, True), 'a zen slice over a hash';
    is-deeply %h, {}, 'and empties it';
}

# --- Negation and the value adverbs --------------------------------------
{
    my %h = a => 1, b => 2;
    is-deeply (%h<a>:!exists:delete), False, ':!exists still deletes';
    is-deeply %h, {b => 2}, 'the key is gone even though the answer is negated';
}
{
    my %h = a => 1, b => 2;
    is-deeply (%h<a>:exists:p:delete), (a => True), ':p shapes the answer';
    is-deeply %h, {b => 2}, 'and the key is still deleted';
}
{
    my %h = a => 1, b => 2;
    is-deeply (%h<a b>:exists:kv:delete), ("a", True, "b", True), ':kv shapes the answer';
    is-deeply %h, {}, 'and both keys are deleted';
}

# --- `:delete` takes a condition -----------------------------------------
{
    my %h = a => 1, b => 2;
    is-deeply (%h<a>:exists:delete(0)), True, 'a falsy :delete argument answers exists';
    is-deeply %h, {a => 1, b => 2}, 'and deletes nothing';
}
{
    my %h = a => 1, b => 2;
    is-deeply (%h<a>:exists:delete(1)), True, 'a truthy :delete argument';
    is-deeply %h, {b => 2}, 'deletes';
}
{
    my %h = a => 1, b => 2;
    is-deeply (%h<a>:exists:!delete), True, ':!delete answers exists';
    is-deeply %h, {a => 1, b => 2}, 'and deletes nothing';
}

# --- Scalar-held containers ----------------------------------------------
{
    my $h = {a => 1, b => 2};
    is-deeply ($h<a>:exists:delete), True, 'a scalar-held hash';
    is-deeply $h, ${b => 2}, 'is mutated in place';
}
{
    my $a = [1, 2, 3];
    is-deeply ($a[0]:exists:delete), True, 'a scalar-held array';
    is-deeply $a, $[Any, 2, 3], 'is mutated in place';
}

# --- Multi-dimensional subscripts ----------------------------------------
# `@a[0;1;2]` reads through a different candidate set. It takes both adverbs
# together, but only with a *positive* `:exists` -- `:!exists:delete` has no
# candidate and is an X::Adverb.
{
    my @a = [[[42, 666, 314],],];
    is-deeply (@a[0;0;0]:exists:delete), True, 'a multidim single index';
    is-deeply @a, [[[Any, 666, 314],],], 'deletes the element it names';
}
{
    my @a = [[[42, 666, 314],],];
    is-deeply (@a[0;0;0]:delete:exists), True, 'and in the other order';
    is-deeply @a, [[[Any, 666, 314],],], 'deletes the same element';
}
{
    my @a = [[[42, 666, 314],],];
    my $c = (0, 1, 2);
    is-deeply (@a[0;0;$c<>]:exists:delete), (True, True, True), 'a multidim slice';
    is-deeply @a, [[[],],], 'deletes every element it names';
}
{
    my @a = [[[42, 666, 314],],];
    throws-like { @a[0;0;0]:!exists:delete }, X::Adverb,
        'a negated multidim :exists cannot delete';
    is-deeply @a, [[[42, 666, 314],],], 'and nothing was deleted';
}
