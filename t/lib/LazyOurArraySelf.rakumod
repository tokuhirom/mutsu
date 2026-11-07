# Fixture for the module-last-statement-lazy-our-array test (#17035): the generator reads
# the very array it builds.
unit module LazyOurArraySelf;
our @S is export = 1, 2, -> $a, $b { @S[0] + $a + $b } ... *;
