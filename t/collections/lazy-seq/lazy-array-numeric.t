use Test;

# A lazy (infinite-backed) array cannot report a finite element count, so every
# numeric/count coercion throws X::Cannot::Lazy (message `Cannot .elems a lazy
# list`) rather than returning the internally-capped backing length. This is the
# L5b follow-up to #3310 (which covered `.elems`).

plan 14;

# --- numeric coercions on an infinite-range array -----------------------
{
    my @a = 1..*;
    throws-like { @a.Int },     X::Cannot::Lazy, '.Int on lazy array throws';
    throws-like { @a.Numeric }, X::Cannot::Lazy, '.Numeric on lazy array throws';
    throws-like { @a.Real },    X::Cannot::Lazy, '.Real on lazy array throws';
    throws-like { @a.end },     X::Cannot::Lazy, '.end on lazy array throws';
    throws-like { +@a },        X::Cannot::Lazy, 'prefix + on lazy array throws';
    throws-like { @a.elems },   X::Cannot::Lazy, '.elems on lazy array throws';
}

# --- the failure is a soft Failure until sunk: // recovers --------------
{
    my @a = 1..*;
    my $n = @a.Int // 'fallback';
    is $n, 'fallback', '.Int Failure is recoverable with //';
}

# --- a finite (non-lazy) array still reports its count normally ---------
{
    my @a = 1, 2, 3, 4;
    is +@a,        4, 'prefix + on finite array is the count';
    is @a.Int,     4, '.Int on finite array is the count';
    is @a.Numeric, 4, '.Numeric on finite array is the count';
    is @a.end,     3, '.end on finite array is last index';
}

{
    my @a = 1..10;
    is +@a,    10, 'prefix + on finite range array';
    is @a.end,  9, '.end on finite range array';
}

# --- an explicitly-bounded range array is NOT lazy ---------------------
{
    my @a = ^5;
    is +@a, 5, 'prefix + on ^N array';
}
