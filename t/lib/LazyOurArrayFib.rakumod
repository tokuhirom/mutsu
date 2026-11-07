# Fixture for the module-last-statement-lazy-our-array test (#17035): a module whose LAST
# statement declares a lazy `our @` array from an infinite sequence.
unit module LazyOurArrayFib;
our @F is export = 1, 1, * + * ... *;
