use MONKEY-TYPING;
use Test;

plan 10;

# `native_lever_a_user_override` — the "has a user `augment` overridden this
# native method?" gate that every native method call on a non-Instance receiver
# passes through — is memoized on `(receiver type name, method)`. The memo is
# only sound if it is dropped whenever the registry changes, so each block below
# calls the native method FIRST (populating the memo with "no override"), then
# augments through `EVAL` (so the augment happens at runtime, after the memo is
# warm), then calls again.
#
# Only methods the class does not declare itself are legal augment targets in
# raku (`augment class Int { method succ {...} }` is a redeclaration error), so
# these use `Array.sort` / `List.first` / `Range.sort`.

{
    my @a = 3, 1, 2;
    is @a.sort.join(','), '1,2,3', 'native Array.sort before the augment';
    EVAL 'use MONKEY-TYPING; augment class Array { method sort { "AUG-SORT" } }';
    is @a.sort, 'AUG-SORT', 'the augment wins on the already-called receiver';
    my @fresh = 9, 8;
    is @fresh.sort, 'AUG-SORT', 'and on a fresh receiver of the same type';
}

# An augment on an MRO *ancestor* must invalidate the memo for the descendant
# receiver too: the gate walks the receiver type's whole MRO, so the memo key is
# the receiver type (`Array`), not the class that declares the method (`List`).
{
    my @a = 5, 6, 7;
    is @a.first(* > 5), 6, 'native Array.first before the augment';
    EVAL 'use MONKEY-TYPING; augment class List { method first(|) { "AUG-FIRST" } }';
    is @a.first(* > 5), 'AUG-FIRST', 'an ancestor augment reaches the descendant receiver';
}

# A different receiver type whose memo was warmed independently.
{
    is (1..3).sort.join(','), '1,2,3', 'native Range.sort before the augment';
    EVAL 'use MONKEY-TYPING; augment class Range { method sort { "AUG-RANGE" } }';
    is (1..3).sort, 'AUG-RANGE', 'augmenting Range is seen after a cached native call';
}

# Methods nobody augmented still resolve natively afterwards — the memo is
# repopulated, not poisoned, and the invalidation is not type-wide-sticky.
is 10.abs, 10, 'an un-augmented native method still works after the invalidations';
is 'abc'.uc, 'ABC', 'and on another receiver type entirely';
is (3, 1, 2).List.sort.join(','), '1,2,3', 'a List receiver keeps its own native sort';
