# Fixture for module-lazy-our-array-shared-list (#17035): the generator reads the array it
# builds (Math::Sequences style), and a `state` counter shows how often it is stepped.
unit module LazyOurArrayShared;
sub infix:<choose>($n, $p) is export(:support) { [*] ($n ... 0) Z/ 1 .. $p }
our @A is export = 0, 1, -> $a {
    state $n = 1;
    ++$n;
    -($n-1) * $a + sum (1 .. ($n - 1)).map: -> $k {
        ($n choose $k) * @A[$k] * @A[$n - $k]
    }
} ... *;
our @Steps is export = 0, -> $a { state $calls = 0; ++$calls; $a + $calls } ... *;
