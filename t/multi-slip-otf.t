use Test;

plan 8;

# §2-D multi-dispatch OTF: a multi call whose arguments are *slipped* at the
# call site (`f(1, |@x)`) is compiled to OpCode::CallFuncSlip, a separate
# dispatch path that lacked the multi-candidate OTF branch. It swallowed multi
# calls into call_function_fallback. The branch now resolves and OTF-compiles
# (or correctly falls back for) the winning candidate, matching the non-slip
# path.

# Slipped args resolve to the right arity candidate.
multi ff($a)      { "one" }
multi ff($a, $b)  { "two" }
my @one = 2;
my @none;
is ff(1, |@one),  'two', 'slip expands to 2-arg candidate';
is ff(1, |@none), 'one', 'empty slip resolves to 1-arg candidate';
is ff(1),         'one', 'no slip still works';

# A genuine capture-param candidate, recursively slipping the capture.
multi with_cap($a)          { $a }
multi with_cap($a, $b, |cap){ with_cap($a + $b, |cap) }
is with_cap(1,2,3,4,5,6), 21, 'recursive |cap multi (S06-multi/syntax)';
is with_cap(10),          10, 'capture multi base case';
is with_cap(3, 4),         7, 'capture multi with empty trailing capture';

# Type-based multi with a slipped typed arg picks the right candidate.
multi tt(Int $a, Int $b) { "ii" }
multi tt(Str $a, Str $b) { "ss" }
my @ints = 2;
my @strs = "y";
is tt(1, |@ints),   'ii', 'slip preserves Int typing for dispatch';
is tt("x", |@strs), 'ss', 'slip preserves Str typing for dispatch';
