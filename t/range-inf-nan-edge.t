use Test;

plan 22;

# Scalar-container Range assigned to an array stays a single item (itemized),
# matching raku's `[1..5,]` rather than flattening.
{
    my $r = 1..5;
    my @r = $r;
    is @r.raku, "[1..5,]", 'scalar-container Range -> single array element';
    is @r.elems, 1, 'and has one element';
}
{
    my $r = 'a'..'c';
    my @r = $r;
    is @r.raku, '["a".."c",]', 'scalar-container Str Range -> single element';
}

# .int-bounds throws for any Inf/-Inf/NaN endpoint (including the i64 sentinel
# ranges `1..Inf` / `1..*` / `-Inf..1`).
for (NaN..NaN, NaN..1, 1..NaN, -Inf..1, 1..Inf, Inf..1, 1..*, Inf..Inf) -> $r {
    dies-ok { $r.int-bounds }, "{$r.raku}.int-bounds throws";
}

# A finite non-int range still has integer bounds.
is-deeply (0..5.5).int-bounds, (0, 5), 'int-bounds of 0..5.5 is (0, 5)';

# The genuine full-i64 range (both extremes present) has concrete bounds — the
# i64::MIN/MAX sentinel only means "open end" when it appears alone.
is-deeply int64.Range.int-bounds, (-9223372036854775808, 9223372036854775807),
    'int64.Range.int-bounds returns the real i64 bounds (not a lazy throw)';

# Inf/NaN range indexing yields Nil; degenerate counts.
is-deeply (Inf .. Inf)[^5], (Nil, Nil, Nil, Nil, Nil), 'Inf..Inf indexes to Nil';
is-deeply (Inf .. NaN)[^5], (Nil, Nil, Nil, Nil, Nil), 'Inf..NaN indexes to Nil';
is-deeply (Inf .. 0).elems, 0, 'Inf..0 is empty';

# Numeric coercion of degenerate infinite ranges is Inf.
is-deeply +(-∞ .. -∞), Inf, '+(-Inf..-Inf) is Inf';
is-deeply +(∞ .. ∞), Inf, '+(Inf..Inf) is Inf';

# A non-finite-start range never advances under .succ, so .map yields its
# start ad infinitum (lazily — bounded here with `last`).
{
    my $got;
    (-Inf..0).map: { next unless $++ > 1050; $got = $_; last }
    is-deeply $got, -Inf, '-Inf..0 .map keeps producing -Inf';
}
{
    my $got;
    (NaN..NaN).map: { next unless $++ > 1050; $got = $_; last }
    is-deeply $got, NaN, 'NaN..NaN .map keeps producing NaN';
}

# An infinite Range cannot initialize a native typed array (X::Cannot::Lazy),
# regardless of which end is unbounded.
{
    my num @arr;
    throws-like { @arr = 0e0..Inf }, X::Cannot::Lazy,
        action => 'initialize', what => 'array[num]', 'right-infinite init throws';
    throws-like { @arr = -Inf..0e0 }, X::Cannot::Lazy,
        action => 'initialize', what => 'array[num]', 'left-infinite init throws';
}
