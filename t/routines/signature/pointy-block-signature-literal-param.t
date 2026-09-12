use Test;

# `-> :($a, $b) { ... }` reads like a signature literal but is not one: rakudo
# parses it as ONE parameter — an anonymous named `$` whose sub-signature is
# `($a, $b)` — so the block's own signature gists as `(:$ ($a, $b))`.
#
# PrettyDump builds its handlers that way:
#
#     -> :(PrettyDump $pretty, $ds, Int:D :$depth = 0 --> Str) { ... }
#
# mutsu's sub-parameter parser has always got this right (`sub (:($a, $b))`),
# but the pointy-block parameter parser is a separate hand-rolled list of
# shapes and had no case for `:(` at all: it accepted `:` only when a sigil
# followed. The construct therefore reached no branch and the whole block
# failed to parse, taking PrettyDump — and Collection / RakuConfig, which
# depend on it — down with it. The pointy parser now delegates this one shape
# to the sub-parameter parser rather than restating it.

plan 9;

# The parameter is an anonymous named one carrying a sub-signature.
{
    my $b = -> :($a, $b) { 42 };
    is $b.signature.gist, '(:$ ($a, $b))',
        'a pointy `:(...)` parameter is an anonymous named param with a sub-signature';
}

# An empty one is optional, so the block is callable with no arguments.
{
    my $b = -> :() { 42 };
    is $b(), 42, 'an empty `:()` parameter leaves the block callable with no args';
}

# The shape PrettyDump actually writes, including a type, a defaulted named
# parameter inside the sub-signature and a return constraint.
{
    class PrettyDumpish { }
    my $c = -> :(PrettyDumpish $pretty, $ds, Int:D :$depth = 0 --> Str) { 'handled' };
    ok $c.defined, 'the PrettyDump handler shape parses';
    ok $c.signature.gist.contains('PrettyDumpish $pretty'),
        'and keeps the sub-signature it was given';
}

# Every other pointy parameter shape must be untouched — the new branch is
# keyed on a literal `:(` prefix and nothing else reaches it.
{
    my $plain = -> $a, $b { $a + $b };
    is $plain(1, 2), 3, 'an ordinary pointy block still works';

    my $destructure = -> [$a, $b] { $a + $b };
    is $destructure([1, 2]), 3, 'positional destructuring still works';

    my $named = -> :$named { $named };
    is $named(:named(3)), 3, 'a named pointy parameter still works';

    my $sigilless = -> Int \v { v };
    is $sigilless(4), 4, 'a typed sigilless pointy parameter still works';

    my $typed-only = -> Int { 'type-only' };
    is $typed-only(1), 'type-only', 'a type-only pointy parameter still works';
}
