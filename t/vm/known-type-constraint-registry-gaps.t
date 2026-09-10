use v6;
use Test;

plan 7;

# `is_known_type_constraint` (src/runtime/utils/type_constraints.rs) is
# consulted by the `?? then !!` ternary guard to decide whether a bare type
# name in then-position is a complete term or the head of a gobbling listop
# call. Several genuine builtin Raku types (Real, Callable, Supply, ...) were
# simply missing from its list, so `1 ?? Real !! 2`-shaped code raised a false
# "Your !! was gobbled" instead of evaluating normally.

is (5 ~~ Int ?? Real !! Str).^name, 'Real', 'Real recognized as a type';
is (5 ~~ Int ?? Callable !! Str).^name, 'Callable',
    'Callable recognized as a type';
is (5 ~~ Int ?? Numeric !! Str).^name, 'Numeric',
    'Numeric recognized as a type';
is (5 ~~ Int ?? Supply !! Str).^name, 'Supply', 'Supply recognized as a type';
is (5 ~~ Int ?? Iterable !! Str).^name, 'Iterable',
    'Iterable recognized as a type';

# A plain (non-`my`) class declared inside a bare block stays bareword-visible
# after the block exits, matching Raku's non-lexical class semantics — the
# parser-time type registry now models this for the same disambiguation
# purpose (roast/S04-exception-handlers/catch.t exercises the runtime side of
# this, which was never broken; this pins the parser-time registry only).
{
    class KeepsVisibleAfterBlock {};
}
is (5 ~~ Int ?? KeepsVisibleAfterBlock !! Str).^name, 'KeepsVisibleAfterBlock',
    'a plain class declared in a now-exited block is still a known type';

# The builtin PromiseStatus-shaped constants are known enum values too.
{
    my $promise = Promise.new;
    $promise.keep(1);
    given $promise.status {
        when Kept { pass 'Kept still matches as a when-clause value' }
        default { flunk 'Kept still matches as a when-clause value' }
    }
}
