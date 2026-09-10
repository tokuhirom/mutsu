use Test;

# A container declaration WITH an initializer used in EXPRESSION position
# (`(my @o = $x)`, e.g. `push @a, my @o = $_`) must RE-ASSIGN and produce a
# FRESH container on every evaluation. It used to initialize only once per
# declaration site (a `__do_decl_init` marker), so in a loop the expression
# froze at the first iteration's value.

plan 8;

# --- the expression value tracks the current initializer each iteration ---
{
    my @seen;
    for ^3 -> $x { @seen.push: (my @o = $x).gist }
    is-deeply @seen, ['[0]', '[1]', '[2]'], 'array (my @o = $x) re-evaluates each iteration';
}
{
    my @seen;
    for ^3 -> $x { @seen.push: (my %o = (k => $x)).<k> }
    is-deeply @seen, [0, 1, 2], 'hash (my %o = ...) re-evaluates each iteration';
}

# --- each iteration yields a FRESH container (push captures distinct arrays) ---
{
    my @a;
    for ^3 { push @a, my @o = $_ }
    is-deeply @a, [[0], [1], [2]], 'push of an inline `my @o = $_` captures independent arrays';
}
{
    my @a;
    for ^3 -> $k { push @a, my %o = (v => $k) }
    is-deeply @a.map(*.<v>).List, (0, 1, 2), 'push of an inline `my %o = ...` captures independent hashes';
}

# --- `Empty, $_` (the S02-types/array.t canary) ---
{
    my @a2;
    for ^5 { push @a2, my @o = Empty, $_ }
    is-deeply @a2, [[0], [1], [2], [3], [4]], 'push @a2, my @o = Empty, $_';
}

# --- copy independence is preserved (`my @c = @src`) ---
{
    my @src = 1, 2, 3;
    my @c = @src;
    @c[0] = 99;
    is-deeply @src, [1, 2, 3], 'my @c = @src does not alias the source';
}

# --- a BARE `my @a` (no initializer) still initializes once per site ---
{
    my @out;
    for ^3 {
        my @acc;
        @acc.push($_);
        @out.push(@acc.elems);
    }
    is-deeply @out, [1, 1, 1], 'bare `my @acc` is fresh each iteration (statement form)';
}

# --- the assignment value is usable directly ---
{
    is (my @r = 1, 2, 3).elems, 3, 'return value of an inline array assignment';
}
