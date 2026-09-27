unit module MultiStashReExport;

# Several multis re-exported by a keyed stash assignment in a BEGIN loop: each
# binding must carry every candidate of its own family, and only that family.
multi sub fam-one(Int $x) { "one-int $x" }
multi sub fam-one(Str $x) { "one-str $x" }

multi sub fam-two(Int $x) { "two-int $x" }
multi sub fam-two(Str $x, Str $y) { "two-str2 $x$y" }

multi sub fam-three() { 'three-0' }
multi sub fam-three(*@a) { "three-n {+@a}" }

my package EXPORT::DEFAULT { }

BEGIN for <&fam-one &fam-two &fam-three> {
    EXPORT::DEFAULT::{$_} = ::($_)
}
