use Test;

# A Mix/MixHash weight is a Real, so combining two weights must use the ordinary
# numeric tower (Int + Rat stays an exact Rat; only a genuine Num makes it a
# Num), not f64 arithmetic. `3.14 + 1` is exactly 4.14, but `3.14f64 + 1.0` is
# not the double nearest 4.14, so the baggy operators used to leak
# 4.140000000000001 into the result.
#
# Bag/BagHash weights are Int and must STAY Int -- that is the control here.

plan 40;

# --- (+) / union addition ------------------------------------------------
is (<b> (+) (b => 3.14).Mix)<b>, 4.14, '(+) list/Mix keeps the Rat exact';
is (<b> ⊎ (b => 3.14).Mix)<b>, 4.14, 'the U+228E spelling agrees';
is ((b => 2.5).Mix (+) (b => 3.14).Mix)<b>, 5.64, '(+) Mix/Mix keeps the Rat exact';
is ((b => 3.14).MixHash (+) (b => 1).Mix)<b>, 4.14, '(+) with a MixHash receiver';
is (<a b c> (+) (a => 2.5, b => 3.14).Mix).gist.contains('4.14'), True,
    'the ticket repro renders b(4.14)';

# --- (-) / difference ----------------------------------------------------
is ((b => 4.14).Mix (-) (b => 1).Mix)<b>, 3.14, '(-) keeps the Rat exact';
is ((b => 4.14).Mix ∖ (b => 1).Mix)<b>, 3.14, 'the U+2216 spelling agrees';
is ((b => 1.1).Mix (-) (b => 3.3).Mix)<b>, -2.2, '(-) with a negative result';

# --- (.) / multiplication ------------------------------------------------
is ((b => 3.14).Mix (.) (b => 1.1).Mix)<b>, 3.454, '(.) keeps the Rat exact';
is ((b => 3.14).Mix ⊍ (b => 1.1).Mix)<b>, 3.454, 'the U+228D spelling agrees';

# --- (^) / symmetric difference ------------------------------------------
is ((b => 4.14).Mix (^) (b => 1).Mix)<b>, 3.14, '(^) keeps the Rat exact';
{
    my $m  = (a => 2.5, b => 3.14).Mix;
    my $m2 = (b => 1.1, c => 2.2).Mix;
    my $m3 = (b => 0.5).Mix;
    is ([(^)] $m, $m2, $m3)<b>, 2.04, 'multi-arg (^) keeps the Rat exact';
}

# --- duplicate-key accumulation while a Mix is built ---------------------
is (a => 1.1, a => 2.2).Mix<a>, 3.3, 'duplicate pair weights add exactly';
is (a => 0.1, a => 0.2).Mix<a>, 0.3, 'and again with a harder pair';

# --- weight read-back types ----------------------------------------------
isa-ok (<b> (+) (b => 3.14).Mix)<b>, Rat, 'a summed decimal weight reads back as a Rat';
isa-ok (<b> (+) (b => 1).Mix)<b>, Int, 'a summed whole weight reads back as an Int';

# --- .total ---------------------------------------------------------------
is (a => 2.5, b => 3.14).Mix.total, 5.64, '.total sums under the numeric tower';
isa-ok (a => 2, b => 3).Mix.total, Int, '.total of whole weights is an Int';
is (a => 2, b => 3).Mix.total, 5, 'and has the right value';
is (a => 1.00000000001).Mix.total, 1.00000000001,
    '.total does not snap a weight that is merely close to a whole number';
is (a => 2.5, b => 3.14).MixHash.total, 5.64, '.total on a MixHash agrees';
is (a => 1.5, b => 2.25).Mix.total, 3.75, '.total of exactly-representable weights';

# --- Bag weights stay Int (the control) ----------------------------------
isa-ok ((a => 2).Bag (+) (a => 3).Bag)<a>, Int, '(+) on Bags keeps an Int weight';
is ((a => 2).Bag (+) (a => 3).Bag)<a>, 5, 'and the right count';
isa-ok ((a => 5).Bag (-) (a => 2).Bag)<a>, Int, '(-) on Bags keeps an Int weight';
isa-ok ((a => 5).Bag (^) (a => 2).Bag)<a>, Int, '(^) on Bags keeps an Int weight';
isa-ok ((a => 5).Bag (.) (a => 2).Bag)<a>, Int, '(.) on Bags keeps an Int weight';
isa-ok (a => 2, b => 3).Bag.total, Int, 'Bag.total is an Int';
is (<a b> (+) <b c>).WHAT.^name, 'Bag', 'Set (+) Set is still a Bag';

# --- a set operator reduction reads its operands' values, not containers --
{
    my $b1 = (a => 3, b => 1).Bag;
    my $b2 = (a => 1).Bag;
    my $b3 = (a => 2).Bag;
    is-deeply ([(^)] $b1, $b2, $b3), (a => 1, b => 1).Bag,
        '[(^)] over $-held Bags reduces them, not treats each as one element';
    is-deeply ([(|)] $b1, $b2, $b3), (a => 3, b => 1).Bag, '[(|)] likewise';
    is-deeply ([(&)] $b1, $b2, $b3), (a => 1).Bag, '[(&)] likewise';
    is-deeply ([(-)] $b1, $b2, $b3), (b => 1).Bag, '[(-)] likewise';
    is-deeply ([(.)] $b1, $b2, $b3), (a => 6).Bag, '[(.)] likewise';
    is-deeply ([(+)] $b1, $b2, $b3), (a => 6, b => 1).Bag, '[(+)] likewise';
}

# --- [(+)] / [U+228E] are reductions, not a parse error -------------------
is-deeply ([(+)] <a b>, <b c>), (a => 1, b => 2, c => 1).Bag,
    '[(+)] parses and reduces as baggy addition';
is-deeply ([⊎] <a b>, <b c>), (a => 1, b => 2, c => 1).Bag,
    'and so does its U+228E spelling';
is ([(+)] (a => 2.5).Mix, (a => 3.14).Mix)<a>, 5.64,
    '[(+)] over Mixes keeps the Rat exact too';

# --- a weight is printed the way it is read back -------------------------
is (a => 2e300).Mix.gist, 'Mix(a(2e+300))',
    'a huge whole weight is not truncated to i64 by the renderer';
is (a => 3.14, b => 2, c => 1, d => -1).Mix.gist, 'Mix(a(3.14) b(2) c d(-1))',
    'weight 1 stays elided and the others render as they read back';
