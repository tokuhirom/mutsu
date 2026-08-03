use Test;

plan 7;

# A placeholder belongs to the INNERMOST block that mentions it, so a nested
# block's `$^a` never conflicts with the enclosing block's explicit signature.
# The conflict check used to scan the body deeply and reported the inner block's
# own placeholder as "cannot override existing signature" — which is what made
# Digest::MD5 and Digest::RIPEMD fail to compile.

{
    my $f = -> $b, $i { ({ $^a + $^b }, { $^a * $^b })[$i](3, 4) };
    is $f(0, 0), 7, 'a paren list of placeholder blocks inside a pointy block';
    is $f(0, 1), 12, 'and its second candidate';
}

{
    my $g = -> $b, $i {
        (
          { ($^a +& $^b) +| (+^$^a +& $^c) },
          { $^a +^ $^b +^ $^c }
        )[$i](|$b)
    };
    is $g((1, 2, 3), 0), 2, 'bitwise placeholder blocks, slipped arguments';
    is $g((1, 2, 3), 1), 0, 'and the xor candidate';
}

{
    my $h = -> $x { (BEGIN Array.new: { $^a + $^b }, { $^a - $^b })[0]($x, 1) };
    is $h(41), 42, 'placeholder blocks inside a BEGIN-built Array';
}

# A placeholder that really does belong to the signature-carrying block is
# still rejected. (A `->` pointy block with a placeholder in its own body is a
# pre-existing gap in mutsu and is deliberately not asserted here.)
eval-dies-ok 'sub bad($x) { $^y }; bad(1)',
    'a placeholder in a sub with an explicit signature is still an error';

{
    my $k = sub ($n) { (1, 2, 3).map({ $^e * $n }).List };
    is $k(2), (2, 4, 6), 'a placeholder inside a map block is the map block\'s own';
}
