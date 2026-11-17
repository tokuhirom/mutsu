use Test;

# A named unary builtin (`lc`, `uc`, `abs`, ...) is a listop: a chain of
# prefix operators glued to its argument opens that argument, so `lc ~?$b` is
# `lc(~?$b)`, not the bareword `lc` concatenated with `?$b` (DataStar's
# `sub js-bool(Bool $raku-bool) { lc ~?$raku-bool }`).

plan 7;

sub js-bool(Bool $b) { lc ~?$b }
is js-bool(True),  'true',  'lc ~?$b, True';
is js-bool(False), 'false', 'lc ~?$b, False';
is (lc ~?True), 'true', 'lc ~?True (prefix glued to a bareword term)';
is (lc ?True), 'true', 'lc ?True';
is (uc ~?False), 'FALSE', 'uc ~?False';
is (abs -3), 3, 'abs -3 still takes its argument';
my $x = 5;
is (abs - $x), 5, 'a spaced prefix is unaffected';
