use v6;
use Test;

# ADR-0044 D1: a local `multi` for one of the seven core listops ADDS a
# candidate to the existing dispatch set instead of REPLACING the core
# array/string behavior (unlike a plain non-multi `sub` of the same name,
# which does replace it — see t/listop-shadow-declared.t). Verified against
# real raku output (docs/adr/0044-listops-are-routines-not-a-syntactic-rewrite.md
# section 2.1).

plan 8;

multi splice(Str $s, Int $i) { "custom $s $i" }
{
    my @a = (1, 2, 3, 4, 5);
    splice(@a, 1, 2);
    is-deeply @a, [1, 4, 5],
        'core array splice still works alongside a local multi splice';
    is splice("x", 1), 'custom x 1',
        'and the local multi splice candidate is reachable too';
}

multi push(Str $s, Int $i) { "custom push $s $i" }
{
    my @a = (1, 2, 3);
    push(@a, 9);
    is-deeply @a, [1, 2, 3, 9],
        'core array push (parens) still works alongside a local multi push';
    is push("x", 1), 'custom push x 1',
        'and the local multi push candidate is reachable too';
    my @b = (1, 2, 3);
    push @b, 10;
    is-deeply @b, [1, 2, 3, 10],
        'core array push (no-parens statement form) still works too';
}

multi pop(Str $s) { "custom pop $s" }
{
    my @a = (1, 2, 3);
    is pop(@a), 3, 'core array pop still works alongside a local multi pop';
    is-deeply @a, [1, 2], 'and the array was actually mutated';
    is pop("x"), 'custom pop x',
        'and the local multi pop candidate is reachable too';
}
