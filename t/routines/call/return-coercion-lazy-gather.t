use Test;

# A return-type coercion to an eager container (`--> Array(Seq)` / `--> Array()`
# / `--> List(...)`) must reify a lazy value (a gather/take coroutine) before
# reading its elements, so the coercion yields the produced items — not an empty
# container. A plain `--> Seq` (no coercion) keeps its laziness.

plan 7;

sub g-array(--> Array(Seq)) { gather { take 2; take 3 } }
is-deeply g-array().List, (2, 3), 'gather through --> Array(Seq) reifies to its items';

sub g-array-empty-src(--> Array()) { gather { take 5; take 6 } }
is-deeply g-array-empty-src().List, (5, 6), 'gather through --> Array() reifies';

# eager Seq return still coerces fine (no regression)
sub eager-seq(--> Array(Seq)) { (7, 8, 9).Seq }
is-deeply eager-seq().List, (7, 8, 9), 'eager Seq through --> Array(Seq) still works';

# the categorize doc example: a multi-key mapper returning Array(Seq). Use Int
# keys (not `<...>` allomorphs) so the bucket keys are plain Int, decoupled from
# the separate object-hash key-identity item (PLAN.md §8.10).
sub divisible-by(Int $n --> Array(Seq)) {
    gather { for 2, 3, 5, 7 { take $_ if $n %% $_ } }
}
my %cat = (3..13).categorize(&divisible-by);
is-deeply %cat{2}.List, (4, 6, 8, 10, 12), 'categorize gather mapper: bucket 2';
is-deeply %cat{3}.List, (3, 6, 9, 12),     'categorize gather mapper: bucket 3';

# a plain --> Seq keeps a lazy gather lazy (laziness is NOT forced)
sub lazy-seq(--> Seq) { lazy gather { take 1; take 2 } }
ok lazy-seq().is-lazy, 'a plain --> Seq return keeps a lazy gather lazy';

# non-lazy value through Array(Seq) coercion is unaffected
sub already-array(--> Array(Seq)) { my @a = 1, 2, 3; @a.Seq }
is-deeply already-array().List, (1, 2, 3), 'a reified array through --> Array(Seq) works';
