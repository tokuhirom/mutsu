use v6;
use Test;

# Issue #8888: a zero-argument native method on a *plain* receiver (a
# non-shaped, non-lazy array; a non-itemized hash; a Str) skips the receiver
# probe gauntlet and jumps straight to the method family cascade, authorized by
# the `(kind, method)` table in `src/builtins/fast_0arg.rs`.
#
# The table only ever removes a walk whose outcome is already known, so every
# assertion here is just "the answer is still the one the full path gives".
# What it pins is that the *shapes the table refuses* keep their own arms: a
# shaped array, a lazy one, a Seq, a Range, an itemized hash and a type object
# all have cascade arms the table would bypass if `dispatch_shape` ever
# widened to admit them.

plan 26;

# --- the authorized pairs, non-empty and empty ------------------------------

my @a = 1..10;
is @a.elems, 10, 'plain Array .elems';
is @a.end, 9, 'plain Array .end';
is @a.Bool, True, 'plain Array .Bool';

my @empty;
is @empty.elems, 0, 'empty Array .elems';
is @empty.end, -1, 'empty Array .end';
is @empty.Bool, False, 'empty Array .Bool';

my $list = (1, 2, 3);
is $list.elems, 3, 'List .elems';
is $list.end, 2, 'List .end';
is $list.Bool, True, 'List .Bool';

my %h = a => 1, b => 2;
is %h.elems, 2, 'plain Hash .elems';
is %h.Bool, True, 'plain Hash .Bool';

my %empty;
is %empty.elems, 0, 'empty Hash .elems';
is %empty.Bool, False, 'empty Hash .Bool';

is "hello".chars, 5, 'Str .chars';
is "".chars, 0, 'empty Str .chars';
is "".Bool, False, 'empty Str is False';
is "0".Bool, True, 'the string "0" is True';
is "abc".Bool, True, 'non-empty Str is True';

# A grapheme cluster is one character: `.chars` must still be the cascade's
# grapheme-aware count, not a byte or codepoint count.
is "e\c[COMBINING ACUTE ACCENT]".chars, 1, 'Str .chars counts graphemes';

# --- shapes the table refuses keep their own arms ---------------------------

# A lazy list cannot report `.elems`: raku throws X::Cannot::Lazy. Reaching
# the plain-array count arm instead would answer a capped backing length.
my $lazy = (1 .. Inf).map(* + 1);
dies-ok { $lazy.elems }, 'a lazy list still refuses .elems';

# A Range counts its own span rather than a backing vector.
is (1 .. 10).elems, 10, 'Range .elems is its span';
is (1 ..^ 10).elems, 9, 'exclusive Range .elems is its span';

# A Seq reifies before it is counted.
is (gather { take 1; take 2 }).elems, 2, 'Seq .elems reifies first';

# A type object is not a plain receiver: raku treats an undefined invocant as
# a one-element list of itself, which is a `Package` arm the table refuses.
is Array.elems, 1, 'Array type object .elems';

# A shaped array reports its declared shape.
my @shaped[3];
is @shaped.elems, 3, 'shaped Array .elems is its shape';

# An itemized aggregate keeps its scalar container's identity.
my $itemized = [1, 2, 3];
is $itemized.elems, 3, 'itemized Array .elems';

done-testing;
