# The associative mirror of `t/subscript-kind-positional.t`. A `<...>` / `{...}`
# subscript of a value that does not do `Associative` follows raku's `Any`
# protocol: `Any.EXISTS-KEY` is always False, so every key is missing, and
# `Any.AT-KEY` fails with "Type X does not support associative indexing.".
#
# The two halves of that model decide the value adverbs between them: a plain
# `:v`/`:k`/`:kv`/`:p` drops the missing key and yields the empty list, while the
# negated `:!v`/`:!k`/`:!kv`/`:!p` must produce a value and so surfaces the
# failure. mutsu used to answer `Nil` to all eight.
#
# Every assertion here also passes unmodified under rakudo.
use Test;
plan 46;

sub assoc-failure($v, $type) {
    ok $v ~~ Failure, "$type: the value is a Failure";
    is $v.exception.message, "Type $type does not support associative indexing.",
        "$type: with the Any.AT-KEY message";
}

# --- the eight value adverbs on an Int ---
{
    is-deeply (5<a>:v),  (), ':v on a non-Associative finds no key';
    is-deeply (5<a>:k),  (), ':k likewise';
    is-deeply (5<a>:kv), (), ':kv likewise';
    is-deeply (5<a>:p),  (), ':p likewise';

    is-deeply (5<a>:!k), "a", ':!k keeps the key it was given';
    assoc-failure((5<a>:!v), 'Int');

    my ($k, $v) = (5<a>:!kv);
    is-deeply $k, "a", ':!kv keeps the key';
    assoc-failure($v, 'Int');

    my $pair = (5<a>:!p);
    is-deeply $pair.key, "a", ':!p keeps the key';
    assoc-failure($pair.value, 'Int');
}

# --- the brace spelling is the same subscript ---
{
    is-deeply (5{"a"}:v), (), '{} is an associative subscript too';
    assoc-failure((5{"a"}:!v), 'Int');
}

# --- a multi-key slice reports one entry per key ---
{
    is-deeply (5<a b>:v), (), 'a slice of missing keys is empty';
    is-deeply (5<a b>:!k), ("a", "b"), 'and keeps both keys when negated';
    is-deeply (5<a b>:!kv).elems, 4, ':!kv pairs each key with its failure';
}

# --- the type in the message is the target's own ---
{
    assoc-failure(("s"<a>:!v), 'Str');
    assoc-failure(((1, 2)<a>:!v), 'List');
    # An Array does Positional, not Associative: `<a>` is a key, never the `-1`
    # a numified positional lookup would report.
    my @a = 1, 2;
    is-deeply (@a<a>:!k), "a", 'an Array key is not numified into an index';
    assoc-failure((@a<a>:!v), 'Array');
}

# --- a type object answers Any instead of failing ---
{
    is-deeply (Any<a>:v), (), 'a type object still finds no key';
    is-deeply (Int<a>:!v), Any, 'but an undefined invocant answers Any, not a Failure';
}

# --- a Pair does Associative, with exactly one key ---
{
    my $p = (:x(1));
    is-deeply ($p<x>:v), 1, 'a Pair reads back its own key';
    is-deeply ($p<y>:v), (), 'and reports any other key as missing';
    is-deeply ($p<y>:!k), "y", ':!k on a Pair keeps the key';
    is-deeply ($p<y>:!v), Nil, 'a Pair reports a missing value as Nil, not a failure';
    is-deeply ($p<x y>:kv), ("x", 1), 'a Pair slice drops the missing key';

    is-deeply $p.AT-KEY("x"), 1, 'Pair.AT-KEY';
    is-deeply $p.EXISTS-KEY("x"), True, 'Pair.EXISTS-KEY';
    is-deeply $p.EXISTS-KEY("y"), False, 'Pair.EXISTS-KEY on a missing key';
}

# --- the same model reached through the methods themselves ---
{
    is-deeply 5.EXISTS-KEY("a"), False, 'Any.EXISTS-KEY is always False';
    assoc-failure(5.AT-KEY("a"), 'Int');
    is-deeply Any.AT-KEY("a"), Any, 'Any:U.AT-KEY answers Any';
}

# --- the positional twin: a scalar read past its one slot ---
{
    # `5[1]` is index 1 of the one-element list holding 5, which is out of range
    # over `0..0` -- a per-index failure, not a container default.
    my $v = (5[1]:!v);
    ok $v ~~ Failure, 'a scalar index past 0 is a Failure';
    is $v.exception.WHAT.^name, 'X::OutOfRange', 'carrying X::OutOfRange';
    is-deeply (5[1]:!k), 1, ':!k still reports the index';
    is-deeply (5[1]:v), (), 'and the un-negated adverb finds nothing';

    # A List has no element type, so its own missing slot is Nil where a real
    # `@`-array reports the Any hole.
    is-deeply ((1, 2)[5]:!v), Nil, 'a List reports a missing slot as Nil';
    my @a = 1, 2;
    is-deeply (@a[5]:!v), Any, 'while an Array reports the Any hole';
}
