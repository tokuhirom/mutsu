use Test;

# Real lazy infinite closure-based sequences (`1, 1, * + * ... *`).
# Elements past the eager prefix are produced on demand by re-invoking the
# generator closure, so indexing far into an infinite sequence works (and
# promotes to BigInt) instead of returning Nil.

plan 16;

# --- scalar-bound Seq, single index ---
{
    my $s = (1, 1, * + * ... *);
    is $s[10], 89, 'fib seq index 10';
    is $s[20], 10946, 'fib seq index 20';
    is $s[40], 165580141, 'fib seq index 40 (past eager prefix)';
    # fib(100) overflows i64: the closure `+` promotes to BigInt automatically.
    is $s[100], 573147844013817084101, 'fib seq index 100 is a BigInt';
}

# --- .lazy-marked array, single index and slice ---
{
    my @a = (1, 1, * + * ... *).lazy;
    is @a[15], 987, '.lazy fib array index 15';
    is @a[^8].join(','), '1,1,2,3,5,8,13,21', '.lazy fib array slice';
}

# --- constant binding indexed deep ---
{
    constant fib := 0, 1, *+* ... *;
    is fib[10], 55, 'constant fib index 10';
    is fib[100], 354224848179261915075, 'constant fib index 100';
}

# --- side-effecting generator keeps state across on-demand pulls ---
{
    my $i = 0;
    my @s = ({ $i++ } ... *).lazy;
    is @s[0..4].join(','), '0,1,2,3,4', 'side-effecting closure generator';
    is @s[10], 10, 'side-effecting closure generator continues on demand';
}

# --- named sub as generator ---
{
    sub add2($a, $b) { $a + $b }
    my $s = (1, 1, &add2 ... *);
    is $s[10], 89, 'named-sub generator index 10';
    is $s[30], 1346269, 'named-sub generator index 30';
}

# --- single-arg closure generator ---
{
    my $s = (1, { $_ + 2 } ... *);
    is $s[100], 201, 'single-arg closure generator deep index';
}

# --- a closure generator that terminates (`last`) stays finite ---
{
    my @s = (1, 2, 3, { last } ... *);
    is @s.elems, 3, 'closure generator that calls last yields a finite list';
    is @s[10]:exists, False, 'no elements past the finite end';
}

# --- the eager prefix is unaffected ---
{
    my $s = (2, 4, * * * ... *);  # multiplicative-ish closure
    is $s[0], 2, 'eager prefix element 0 intact';
}
