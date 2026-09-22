use Test;

# A meta-operator's inner operator is decoded ONCE (InfixShape::lower) and the
# decoded shape is what every element application walks. These pin that the
# per-element semantics did not move with the decode: each composed form below
# would break if a layer were peeled at the wrong time, applied once instead of
# per element, or lost on the way into a nested level.

plan 23;

my @a = 1, 2, 3;
my @b = 10, 20, 30;

# --- one layer, applied per element -----------------------------------------
is (@a Z+ @b).join(','), '11,22,33', 'Z+ applies the inner op per pair';
is (@a X+ @b).join(','), '11,21,31,12,22,32,13,23,33', 'X+ applies per pair';
is (@a >>+<< @b).join(','), '11,22,33', 'hyper applies per pair';
is (@a Z @b).map(*.join('-')).join(','), '1-10,2-20,3-30', 'bare Z zips into tuples';

# --- the reverse layer swaps the operands, per element, not the list ---------
is (@a Z- @b).join(','), '-9,-18,-27', 'Z- is left minus right';
is (@a RZ- @b).join(','), '9,18,27', 'RZ- swaps each pair';
is ([R-] 10, 3, 2).gist, '-11', 'R on a reduction reverses the fold';

# --- two layers compose ------------------------------------------------------
is ([Z+] (1,2,3), (10,20,30)).join(','), '11,22,33', '[Z+] reduces with a zip inner';
is ([Z] (1,2,3), (10,20,30)).map(*.join('-')).join(','), '1-10,2-20,3-30',
    '[Z] folds into tuples';
is ([\Z~] <a b c>, <1 2 3>).map(*.join('|')).join(','), 'abc,a1|b2|c3',
    'a triangle Z~ recomputes each prefix';

# --- the Unicode aliases survive the decode ---------------------------------
is ([×] @a).gist, '6', 'a Unicode reduction folds to its ASCII operator';
is (@a >>×<< @b).join(','), '10,40,90', 'a Unicode hyper folds too';

# --- the leaf-only special cases stay leaf-only ------------------------------
my @c = 1, 2, 3;
@c »=» 9;
is @c.join(','), '9,9,9', 'a hyper assignment distributes its right operand';
is (@a >>~~>> @a).join(','), 'True,True,True', 'a hyper smartmatch matches per element';
is ((1, 2) Z~~ (1, 3)).join(','), 'True,False', 'Z~~ smart-matches per pair';

# --- meta assignment forms ---------------------------------------------------
my @d = 1, 2, 3;
@d Z+= @b;
is @d.join(','), '11,22,33', 'Z+= folds each right element into its left cell';
# --- a user-defined infix is still resolved at run time ----------------------
sub infix:<myop>($x, $y) { $x * 100 + $y }
is ([myop] 1, 2, 3).gist, '10203', 'a user infix reduces';
is ((1, 2) Zmyop (3, 4)).join(','), '103,204', 'a user infix zips';

# --- nested levels each get the same operator -------------------------------
is ((1, (2, 3), 4) >>+>> 10).gist, '(11 (12 13) 14)', 'a hyper recurses into a nested list';
is ((%(:a(1), :b(2)) >>+>> 1)<a b>).join(','), '2,3', 'a hyper recurses into a Hash';
is (((a => 1) >>*>> 3)).gist, 'a => 3', 'a hyper descends into a Pair value';

# --- junction threading happens at the leaf ---------------------------------
ok ((1|2) Z+ (1,)).head ~~ Junction, 'a junction operand threads through a zip';

# --- a lazy triangle scan decodes once for the whole batch ------------------
is ([\+] (1 .. *))[^5].join(','), '1,3,6,10,15', 'a lazy scan folds its operator per step';
