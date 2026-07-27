use v6;
use lib 't/lib';
use Test;

plan 12;

{
    my @a = 1;
    is push(@a, 2), 'mine:2', 'a block-local sub is hoisted over builtin push';
    is-deeply @a, [1], 'the hoisted user push does not trigger builtin mutation';

    my sub push(@target, *@values) { 'mine:' ~ @values.join(',') }

    push @a, 3;
    is-deeply @a, [1], 'a statement call also resolves to the block-local sub';

    {
        is push(@a, 4), 'mine:4', 'the shadow is inherited by a nested block';
        is-deeply @a, [1], 'the nested call does not trigger builtin mutation';
    }
}

my @outside = 1;
is-deeply push(@outside, 2), [1, 2], 'the builtin is restored outside the block';
is-deeply @outside, [1, 2], 'the restored builtin mutates its array';

{
    use ListopShadow;

    my @imported = 1;
    is push(@imported, 42), 2, 'a block-local import shadows builtin push';
    is-deeply @imported, [1, 42], 'the imported push performs its own mutation';

    my @popped = 7;
    is pop(@popped), 7, 'a block-local import shadows builtin pop';
    is pop(@popped), Nil, 'the imported empty-pop behavior is preserved';
}

my @after_import = 1;
is-deeply push(@after_import, 2), [1, 2], 'a block-local import does not leak';
