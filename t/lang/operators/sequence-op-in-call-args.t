use Test;

# The sequence operator (`...`/`…`) has "list infix" precedence, which is LOOSER
# than comma. So in an argument list `a, b ... limit`, the whole comma list `a, b`
# is the sequence's seed and the entire list collapses into ONE sequence argument,
# exactly like `(a, b ... limit)` in parentheses. Previously mutsu split the comma
# list first, so `f(1, 3 ... 11)` was parsed as `f(1, (3 ... 11))` (seed `3`) or
# dropped the endpoint entirely. Results are compared as flattened, joined strings
# to avoid Array/List/Seq `.raku` representation differences.

plan 15;

sub flat-str(*@a) { @a.join(",") }
sub arg-count(**@a) { @a.elems }

# --- parenthesized function call ---
is flat-str(1, 3 ... 11), "1,3,5,7,9,11", 'paren call: seed is the whole comma list';
is flat-str(2, 4 ... 10), "2,4,6,8,10",   'paren call: even step';
is flat-str(1 ... 3, 9),  "1,2,3,9",      'paren call: single seed, extra endpoint';
is flat-str(1, 3 ... 11, 20), "1,3,5,7,9,11,20", 'paren call: trailing extra after endpoint';
is flat-str(1 ... 3, 5 ... 7), "1,2,3,5,7", 'paren call: chained waypoint sequences';
is flat-str(1, 3 ...^ 11), "1,3,5,7,9",   'paren call: exclusive endpoint';

# The whole sequence is ONE argument, not several.
is arg-count(1, 3 ... 11), 1, 'sequence in a call is a single argument';
is arg-count(1 ... 3, 9),  1, 'single-seed sequence with an extra is a single argument';

# --- unparenthesized listop / IO statement ---
is (1, 3 ... 11).join(","), "1,3,5,7,9,11", 'parenthesized sequence still works';

# --- method colon-arg form: `.meth: a, b ... limit` ---
my @a = <a b c>;
@a.prepend: 1, 3 ... 11;
is @a.join(","), "1,3,5,7,9,11,a,b,c", 'method colon-arg: prepend flattens the sequence';

my @c = <x y>;
@c.append: 2, 4 ... 8;
is @c.join(","), "x,y,2,4,6,8", 'method colon-arg: append flattens the sequence';

# --- method paren-arg form ---
my @d = <a b>;
@d.prepend(1, 3 ... 7);
is @d.join(","), "1,3,5,7,a,b", 'method paren-arg: prepend flattens the sequence';

# --- ordinary comma lists are unaffected (no regression) ---
is flat-str(1, 2, 3), "1,2,3", 'plain comma list is unchanged';
is flat-str(1), "1", 'single argument is unchanged';
is arg-count("a", "b", "c"), 3, 'plain string args stay separate arguments';
