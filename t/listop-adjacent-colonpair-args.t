use Test;

# A colonpair that directly follows another argument with no comma between them
# is a further argument of the SAME no-paren call, and the argument list
# continues past it: `f :a:b, $x` means `f(:a, :b, $x)`.
#
# Regression: only the first colonpair became an argument; the rest were glued
# on by the postfix call-adverb rule, which cannot resume the argument list, so
# the following comma was read by the enclosing list-expression parser and
# `f :a:b, "x", "y"` silently parsed as the list `(f(:a, :b), "x", "y")`.

plan 19;

sub collect(*@p, *%n) {
    @p.join('|') ~ ' / ' ~ %n.sort(*.key).map({ .key ~ '=' ~ .value }).join(',')
}

# --- baseline: forms that always worked ------------------------------------

is collect(:a, 'x'), 'x / a=True', 'single adverb, paren call';
is (collect :a, 'x'), 'x / a=True', 'single adverb before a positional';
is (collect :a, :b, 'x'), 'x / a=True,b=True', 'comma-separated adverbs before a positional';
is (collect :a:b), ' / a=True,b=True', 'chained adverbs with no positional';

# --- the bug: chained adverbs followed by positionals ----------------------

is (collect :a:b, 'x'), 'x / a=True,b=True', 'chained adverbs then one positional';
is (collect :a:b, 'x', 'y'), 'x|y / a=True,b=True', 'chained adverbs then two positionals';
is (collect :a :b, 'x', 'y'), 'x|y / a=True,b=True',
    'space-separated adjacent adverbs then positionals';
is (collect :a:b:c, 'x', 'y'), 'x|y / a=True,b=True,c=True',
    'three chained adverbs then positionals';
is (collect :!d:r, 'x'), 'x / d=False,r=True', 'negated chained adverb then a positional';

# --- adverbs carrying values ----------------------------------------------

is (collect :a<1>:b<2>, 'x'), 'x / a=1,b=2', 'valued chained adverbs then a positional';
is (collect :a(1):b(2), 'x', 'y'), 'x|y / a=1,b=2',
    'paren-valued chained adverbs then positionals';

my $v = 7;
is (collect :a:$v, 'x'), 'x / a=True,v=7', 'variable adverb chained after a boolean adverb';

# --- adverbs after a positional, and in the middle -------------------------

is (collect 'x', :a:b), 'x / a=True,b=True', 'chained adverbs after a positional';
is (collect 'x', :a:b, 'y'), 'x|y / a=True,b=True', 'chained adverbs between positionals';

# --- the whole call must still be ONE expression ---------------------------

my $one = collect :a:b, 'x', 'y';
is $one, 'x|y / a=True,b=True', 'the call is a single expression, not a list';

is (collect :a:b, 'x' xx 2), 'x|x / a=True,b=True',
    'chained adverbs then a repeated positional';

# --- the three no-paren call flavours each have their own arg-list loop -----

# A bareword that is not yet a declared routine at the call site (forward
# reference) takes the generic listop fallback.
my $fwd = forward-ref :a:b, 'x', 'y';
is $fwd, 'x|y / a=True,b=True', 'forward-referenced sub: chained adverbs then positionals';
sub forward-ref(*@p, *%n) { collect(|@p, |%n) }

# A builtin listop (`chdir`) has its own argument-list loop. A misparse turns
# the call into a two-element list, so pin the element count as well.
my $cwd = $*CWD.Str;
my @chdir-result = (chdir :!d:r, $cwd);
is @chdir-result.elems, 1, 'builtin listop with chained adverbs is ONE call, not a list';
is $*CWD.Str, $cwd, 'builtin listop with chained adverbs got the positional path argument';

done-testing;
