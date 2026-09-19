use Test;

plan 6;

my @items = Test::EXPORT::DEFAULT::;

ok @items.elems > 0, 'a package Stash has visible entries in list context';
ok @items.map({ .^name }).grep(* ne 'Pair').elems == 0,
    'Stash list context yields Pairs';
is @items.grep({ .key eq '&ok' }).elems, 1,
    'Stash entries preserve their sigiled keys';
is Test::EXPORT::DEFAULT::.pairs.elems, @items.elems,
    'Stash.pairs yields the same entries as list context';
is Test::EXPORT::DEFAULT::.kv.elems, @items.elems * 2,
    'Stash.kv yields each key and value';
is Test::EXPORT::DEFAULT::.grep(*.key eq '&ok').elems, 1,
    'Stash.grep iterates Pairs';

# Regression source: Test::Coverage re-exports Test with
# `BEGIN EXPORT::DEFAULT::{.key} := .value for Test::EXPORT::DEFAULT::`.
