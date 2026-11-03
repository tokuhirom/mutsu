use v6;
use Test;
use lib 't/lib';
use TermSeedHook;

# A `sub EXPORT` hook returns a package head and a term qualified under it
# ('TermSeedHub' and 'TermSeedHub::Leaf'). A block-scoped `use` of the hook
# declares both as terms; the qualified one was written through the head
# term as a `constant` alias of a package, so the block's exit restored the
# program's own term to its declaration seed and `TermSeedHub::Leaf` read as
# `Any` (Graph::RandomMaze, #16283).
plan 4;

is TermSeedHub::Leaf.^name, 'TermSeedHub::Leaf', 'the top-level use binds the qualified term';
{
    use TermSeedHook;
    is TermSeedHub::Leaf.^name, 'TermSeedHub::Leaf', 'the block sees it too';
}
is TermSeedHub::Leaf.^name, 'TermSeedHub::Leaf', 'the program keeps its term after the block';
is TermSeedHub.^name, 'TermSeedHub', 'the head term is intact';
