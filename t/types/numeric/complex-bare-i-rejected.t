use Test;

# Found via Text::SubParsers 0.1.4's backtracking-with-code-assertion
# candidate scan, which relies on `Str.Complex` rejecting a mid-word
# substring like "wi" (from "with") that ends in a bare "i".
#
# `parse_raku_str_to_numeric`'s `try_parse_complex` (src/runtime/str_numeric.rs)
# defaulted a bare "i"/"+i"/"-i" imaginary part -- with no numeric
# coefficient before it -- to a coefficient of 1 ("i" -> 0+1i, "2+i" ->
# 2+1i). rakudo does not: none of these parse as a Complex at all, they
# fail exactly like any other non-numeric string. mutsu's own comment
# said "rakudo skips these -- cannot handle lone i yet", but that undersold
# it: rakudo actively rejects the bare form, it does not merely decline to
# support it.

plan 12;

# A numeric coefficient before "i" still works (unaffected).
is "3i".Complex, 0+3i,       '"3i".Complex -> 0+3i (unaffected)';
is "2+3i".Complex, 2+3i,     '"2+3i".Complex -> 2+3i (unaffected)';
is "2-3i".Complex, 2-3i,     '"2-3i".Complex -> 2-3i (unaffected)';

# A bare "i" with no coefficient must NOT parse as a number.
dies-ok { "i".Complex },     '"i".Complex dies (no implicit coefficient of 1)';
dies-ok { "+i".Complex },    '"+i".Complex dies';
dies-ok { "-i".Complex },    '"-i".Complex dies';
dies-ok { "2+i".Complex },   '"2+i".Complex dies (bare imaginary part)';
dies-ok { "2-i".Complex },   '"2-i".Complex dies';

# The same rule applies to the general numeric-coercion path (.Numeric),
# not just the explicit .Complex method.
dies-ok { "i".Numeric },     '"i".Numeric dies too (shared parser)';

# A string that merely ends in "i" but isn't numeric-shaped at all stays
# a plain non-numeric failure either way.
dies-ok { "with".Numeric },  '"with".Numeric dies (not number-shaped)';

# Sanity: the general numeric parser used by .Complex still finds a
# genuine complex number inside a value coming from user code.
is "78+7.3i".Complex, 78+7.3i, '"78+7.3i".Complex -> 78+7.3i';

# A trailing lone "i" after a real number that would otherwise be valid
# on its own doesn't retroactively borrow that number's sign as its
# coefficient -- there is no number at all here to combine with.
dies-ok { "-i".Numeric },    '"-i".Numeric dies (not 0-1i)';
