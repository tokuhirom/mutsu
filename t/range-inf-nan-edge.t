use Test;

plan 17;

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

# Inf/NaN range indexing yields Nil; degenerate counts.
is-deeply (Inf .. Inf)[^5], (Nil, Nil, Nil, Nil, Nil), 'Inf..Inf indexes to Nil';
is-deeply (Inf .. NaN)[^5], (Nil, Nil, Nil, Nil, Nil), 'Inf..NaN indexes to Nil';
is-deeply (Inf .. 0).elems, 0, 'Inf..0 is empty';

# Numeric coercion of degenerate infinite ranges is Inf.
is-deeply +(-∞ .. -∞), Inf, '+(-Inf..-Inf) is Inf';
is-deeply +(∞ .. ∞), Inf, '+(Inf..Inf) is Inf';
