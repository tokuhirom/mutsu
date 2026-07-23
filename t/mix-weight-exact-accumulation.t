use v6;
use Test;

plan 12;

# Mix construction accumulates repeated-key weights with EXACT rational
# arithmetic, so `0.1 + 0.02` sums to `0.12`, not the lossy f64
# `0.12000000000000001`. (Regression: mutsu stored/accumulated weights as f64.)

# .Mix coercion on a list of pairs (doc: Type/Mix.rakudoc, Type/Baggy.rakudoc)
{
    my $recipe = (butter => 0.22, sugar => 0.1, flour => 0.275, sugar => 0.02).Mix;
    is $recipe<sugar>, 0.12, '.Mix accumulates 0.1 + 0.02 exactly to 0.12';
    is $recipe<sugar>.raku, '0.12', '.Mix sugar weight raku is 0.12 (not f64 noise)';
    is $recipe.pairs.sort.gist,
        '(butter => 0.22 flour => 0.275 sugar => 0.12)',
        '.Mix pairs render exact weights';
    is $recipe.total, 0.615, '.Mix total is exact';
}

# Mix.new-from-pairs (doc: Type/Baggy.rakudoc [1])
{
    my $m = Mix.new-from-pairs: 'butter' => 0.22, 'sugar' => 0.1, 'sugar' => 0.02;
    is $m.gist, 'Mix(butter(0.22) sugar(0.12))',
        'Mix.new-from-pairs accumulates exactly';
    is $m<sugar>, 0.12, 'new-from-pairs sugar weight is 0.12';
}

# MixHash coercion preserves the same exactness.
{
    my $mh = (a => 0.1, a => 0.02).MixHash;
    is $mh<a>, 0.12, '.MixHash accumulates 0.1 + 0.02 exactly';
}

# Replicated pairs accumulate exactly (0.1 * 3 == 0.3, not 0.30000000000000004).
{
    my $m = ((a => 0.1) xx 3).Mix;
    is $m<a>, 0.3, '.Mix of 3x 0.1 is exactly 0.3';
}

# Integer and mixed weights still work.
{
    my $m = (a => 2, a => 3, b => 1).Mix;
    is $m<a>, 5, 'integer weights accumulate';
    is $m<b>, 1, 'single integer weight';
}

# A single non-accumulated fractional weight is unchanged.
{
    my $m = (x => 0.333).Mix;
    is $m<x>, 0.333, 'single fractional weight preserved';
}

# Negative weights survive accumulation.
{
    my $m = (a => 0.5, a => -0.2).Mix;
    is $m<a>, 0.3, 'accumulation with a negative weight is exact';
}
