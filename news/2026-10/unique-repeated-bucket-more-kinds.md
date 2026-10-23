# `unique` and `repeated` bucket Rat, Complex, Pair, containers and objects

`.unique` / `.repeated` kept the elements already seen in a hash index, but only
for Int, Str, Bool and Num; every other kind was compared against every earlier
value, so a list of `Rat`s, `Pair`s or objects was quadratic (#9161). The index
now also buckets Rat, FatRat, Complex, Pair, Array, Hash, Date/DateTime and plain
objects, each by exactly the data `===` compares for that kind (an object by its
id, or by the `WHICH` its class overrides it with; two distinct Arrays by node).
The identity predicate still runs on every bucket hit, and the kinds that have a
cross-kind identity rule (Seq, Slip, Enum, Set/Bag/Mix, routines, Version) keep
the scan.

`scripts/array-complexity-check.sh 'unique of Rats'` (release): 4,000 Rats
0.5963 s -> 0.0013 s; at 80,000 Rats the doubling ratio is 1.98-2.22
(it was 3.78 at 4,000).
