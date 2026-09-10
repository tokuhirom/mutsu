use Test;

# A `...` sequence-operator result handed directly to a builtin listop used
# to collapse to its un-reified eager prefix: `join` saw only the seed,
# `sum` saw nothing, `min`/`max` returned the whole sequence as a single
# candidate, and `sort` either refused it as "lazy" or dropped it entirely.
# The sequence itself was always correct (`say (...)`, `.join`, `elems (...)`
# all worked) -- only a PURE native listop reading a still-deferred closure
# `...` sequence (a finite generator whose endpoint had not yet been proven
# reached) was affected. See https://github.com/tokuhirom/mutsu/issues/7753
plan 15;

# --- the three repro lines ---
is join(", ", ("az", *.succ ... "bc")), "az, ba, bb, bc",
    'join sees every element of a closure sequence';
is join(",", (1, *+1 ... 4)), "1,2,3,4",
    'join sees every element of an arithmetic-looking closure sequence';
is sum((1, *.succ ... 4)), 10, 'sum adds every element, not zero';

# --- controls that were already correct must stay correct ---
is ("az", *.succ ... "bc").join(","), "az,ba,bb,bc",
    'method-call form is unaffected';
my @a = ("az", *.succ ... "bc");
is join(", ", @a), "az, ba, bb, bc", 'via an array is unaffected';
is elems((1, *.succ ... 4)), 4, 'elems is unaffected';
sub slurpy(*@a) { @a.elems }
is slurpy((1, *.succ ... 4)), 4, 'user slurpy binder is unaffected';

# --- sweep the other builtin listops that take a list argument ---
is-deeply grep({ $_ > 1 }, (1, *+1 ... 4)).List, (2, 3, 4).List,
    'grep flattens a closure sequence argument';
is-deeply map({ $_ * 10 }, (1, *+1 ... 4)).List, (10, 20, 30, 40).List,
    'map flattens a closure sequence argument';
is min((1, *+1 ... 4)), 1, 'min reduces every element of a closure sequence';
is max((1, *+1 ... 4)), 4, 'max reduces every element of a closure sequence';
is-deeply sort((4, *-1 ... 1)).List, (1, 2, 3, 4).List,
    'sort flattens and sorts a closure sequence argument';

# --- lazy-safety: an infinite sequence must not be reified by the fix ---
sub f(*@a) { }
lives-ok { f(1..Inf) }, 'binding an infinite range into a user slurpy stays lazy';
my @inf = 1..Inf;
is-deeply @inf[^3].List, (1, 2, 3).List, 'a lazy array stays lazy after the fix';
throws-like { sort (1, 1, *+* ... *) }, X::Cannot::Lazy,
    'sort on a genuinely infinite closure sequence still throws';

done-testing;
