use Test;

# Pin for the ledger §D(b) slice that drains the case-insensitive named forms of
# Str.starts-with / .ends-with / .substr-eq to VM-native dispatch. The bare forms
# (`starts-with($needle)`, `substr-eq($needle, Int $pos)`) were already native; the
# `:i`/`:ignorecase` markings push them past the arity-keyed native dispatch (a Pair
# arg), so they previously bounced to the interpreter. `:m`/`:ignoremark` (strip_marks)
# and out-of-range positions stay on the interpreter.

plan 21;

# --- starts-with ---------------------------------------------------------------
is "Hello".starts-with("HE", :i), True, 'starts-with :i present';
is "Hello".starts-with("HE"), False, 'starts-with case-sensitive';
is "Hello".starts-with("HE", :!i), False, 'starts-with :!i case-sensitive';
is "Hello".starts-with("xx", :i), False, 'starts-with :i absent';
is "café".starts-with("CAF", :i), True, 'starts-with :i unicode';
is "Hello".starts-with("he", :ignorecase), True, 'starts-with :ignorecase';

# --- ends-with -----------------------------------------------------------------
is "Hello".ends-with("LO", :i), True, 'ends-with :i present';
is "Hello".ends-with("LO"), False, 'ends-with case-sensitive';
is "Hello".ends-with("xx", :i), False, 'ends-with :i absent';
is "Hello".ends-with("lo", :ignorecase), True, 'ends-with :ignorecase';

# --- substr-eq (named, with and without position) ------------------------------
is "foobar".substr-eq("OOB", 1, :i), True, 'substr-eq pos + :i present';
is "foobar".substr-eq("OOB", 1), False, 'substr-eq pos case-sensitive';
is "foobar".substr-eq("OOB", 2, :i), False, 'substr-eq pos + :i mismatch';
is "foobar".substr-eq("oob", "1"), True, 'substr-eq string position';
is "foobar".substr-eq("BAR", 3, :ignorecase), True, 'substr-eq :ignorecase';
is "foobar".substr-eq("FOO", 0, :i), True, 'substr-eq pos 0 + :i';
is "foobar".substr-eq("foo", :i), True, 'substr-eq :i no explicit position';

# --- error / fall-through cases preserved --------------------------------------
{
    my $r = "foo".substr-eq("o", -1, :i);
    ok $r ~~ Failure, 'substr-eq negative position -> Failure';
    $r.so;
}

# --- Match invocant still works (interpreter coercion path) ---------------------
my $m = "Foobar".match(/\w+/);
is $m.starts-with("foo", :i), True, 'Match invocant starts-with :i';
is $m.ends-with("BAR", :i), True, 'Match invocant ends-with :i';
is $m.substr-eq("oob", 1, :i), True, 'Match invocant substr-eq :i';
