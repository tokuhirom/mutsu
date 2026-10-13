use Test;

# ADR-11276 §9.100: `combinations($of)` and `batch($n)` / `batch(:$elems!)`
# are rows of `Any` (Rakudo's `self.list.METHOD`), which a `Seq`, a `Range`,
# a `Str` and the other closed shapes reach, so the one-argument cascade arms
# that answered them are gone. The answers below are Rakudo's.

plan 14;

is (1..3).combinations(2).raku, '((1, 2), (1, 3), (2, 3)).Seq', 'Range.combinations';
is (1, 2, 3).Seq.combinations(2).raku, '((1, 2), (1, 3), (2, 3)).Seq', 'Seq.combinations';
is (1..3).combinations(1..2).elems, 6, 'Range.combinations(Range)';
is-deeply "abc".combinations(2).List, (), 'Str.combinations is over one element';
is (a => 1).combinations(1).elems, 1, 'Pair.combinations';
is Str.combinations(1).elems, 1, 'a type object answers Any.combinations';
throws-like { (1..*).combinations(2).List }, X::Cannot::Lazy,
    'an infinite Range refuses combinations';

is (1, 2, 3, 4).Seq.batch(2).raku, '((1, 2), (3, 4)).Seq', 'Seq.batch';
is (1..5).batch(2).raku, '((1, 2), (3, 4), (5,)).Seq', 'Range.batch';
is set(1).batch(1).elems, 1, 'Set.batch';
is (1..*).batch(2).head(2).raku, '((1, 2), (3, 4)).Seq', 'a lazy Range batches lazily';
is (1..4).batch(:elems(2)).raku, '((1, 2), (3, 4)).Seq', 'batch(:elems)';
dies-ok { (1, 2).batch() }, 'batch() without a size binds no candidate';

is <1.5>.round(0.5), 1.5, 'an allomorph reaches Cool.round($scale)';
