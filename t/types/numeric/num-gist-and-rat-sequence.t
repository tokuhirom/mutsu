use Test;

plan 16;

# --- Num.gist renders Inf/-Inf/NaN and large/small magnitudes like .Str ---
is Inf.gist,   'Inf',   'Inf.gist';
is (-Inf).gist, '-Inf', '-Inf.gist';
is NaN.gist,   'NaN',   'NaN.gist';
is (1e20).gist, '1e+20', 'large magnitude gist uses scientific notation';
is (1e15).gist, '1e+15', '1e15 gist uses scientific notation';
is (0.1).gist, '0.1',   'ordinary Num gist';
is (100e0).gist, '100', 'integer-valued Num gist';
# gist and Str agree for a Num (Raku semantics)
is (1e20).gist, (1e20).Str, 'Num gist == Str (large)';
is Inf.gist, Inf.Str, 'Num gist == Str (Inf)';

# --- an arithmetic sequence with a rational step stays exact (Rat), not float ---
{
    is (0, 1/10 ... 1).gist, '(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1)',
        'rational-step arithmetic series is exact';
    my @s = (0, 1/10 ... 1);
    isa-ok @s[3], Rat, 'element is a Rat, not a Num';
    is @s[3], 3/10, 'element equals the exact rational';
}
{
    is (0, 1/3 ... 1)[2], 2/3, 'third element of 1/3-step series is exactly 2/3';
}

# --- an integer-step sequence stays Int (no Rat regression) ---
{
    my @i = (1, 2 ... 5);
    isa-ok @i[2], Int, 'integer-step sequence stays Int';
    is (1, 2 ... 5).gist, '(1 2 3 4 5)', 'integer sequence renders as integers';
}

# --- a genuinely float sequence is unaffected ---
{
    isa-ok (0e0, 0.5e0 ... 2e0)[1], Num, 'Num-seeded sequence stays Num';
}
