use Test;

# The :x adverb to .match must be an Int or a Range. Passing anything else
# (e.g. an Array) throws X::Str::Match::x carrying the offending value in .got.

plan 6;

throws-like '"a".match(:x([1, 2, 3]), /a/).Str', X::Str::Match::x, got => Array;
throws-like '"a".match(:x({:a}), /a/).Str', X::Str::Match::x, got => Hash;

# Valid :x arguments still work.
is "aaa".match(:x(2), /a/).elems, 2, ':x with an Int selects N matches';
is "aaaa".match(:x(2..3), /a/).elems, 3, ':x with a Range selects within bounds';
is "aaa".match(:x(*), /a/).elems, 3, ':x(*) selects all matches';

# Too few matches for :x(N) yields no match (not an error).
ok "a".match(:x(2), /a/) === Nil || !"a".match(:x(2), /a/), ':x with too few matches is Nil';
