use v6;
use Test;

# A user `augment class Array/List/... { method NAME {...} }` overriding a
# builtin "lever A" method (sort/map/first/min/max/minmax/Set/...) must win
# over the native fast path. These methods are legal augment targets in raku
# because Array/List/... do not declare their own copy of them (unlike e.g.
# `augment class Str { method uc {...} }`, which raku rejects as a
# redeclaration) — so this is not the same "illegal program" class as that
# one, and must actually dispatch to the user override.
#
# `augment` takes effect at compile time in raku (not at its runtime lexical
# position), so every augment for this file lives up front, before any
# assertion runs — do not interleave "plain" and "augmented" checks in the
# same file/process.

use MONKEY-TYPING;
augment class Array {
    method sort { "USER-SORT-OVERRIDE" }
}
augment class Array {
    method map($f) { "USER-MAP-OVERRIDE" }
}
augment class List {
    method first { "USER-FIRST-OVERRIDE" }
}

plan 4;

my @a = (3, 1, 2);
is @a.sort, "USER-SORT-OVERRIDE", 'augmented Array.sort wins over native .sort (mut receiver)';
is (3, 1, 2).Array.sort, "USER-SORT-OVERRIDE",
    'augmented Array.sort wins over native .sort (non-mut receiver)';
is @a.map({ $_ }), "USER-MAP-OVERRIDE", 'augmented Array.map wins over native .map';
is @a.first, "USER-FIRST-OVERRIDE", 'augmented List.first wins over native .first';

done-testing;
