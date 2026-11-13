use Test;

# A `multi infix:<~>` (or `&&`, `||`, `^^`, `//`) with no precedence trait adds
# a candidate to the core operator, which keeps its own precedence: it is not a
# new operator at the trait-less additive level (Red exports `multi infix:<~>`,
# after which `'b' x 3 ~ 'a'` read as `'b' x (3 ~ 'a')`).

plan 9;

class P { }
multi infix:<~>(P $a, P $b) { 'user ~' }
multi infix:<&&>(P $a, P $b) { 'user &&' }
multi infix:<||>(P $a, P $b) { 'user ||' }
multi infix:<//>(P $a, P $b) { 'user //' }

is 'b' x 3 ~ 'a', 'bbba', '~ stays looser than x';
is 1 + 2 ~ 3 + 4, '37', '~ stays looser than +';
is P.new ~ P.new, 'user ~', 'the declared candidate is called';
is 1 + 2 && 3 + 4, 7, '&& stays looser than +';
is 0 + 0 || 3 + 4, 7, '|| stays looser than +';
is Int // 3 + 4, 7, '// stays looser than +';
is 2 x 2 && 3, 3, '&& stays looser than x';
is ('a' ~ 'b' x 2), 'abb', 'x binds tighter than ~ on its right';
is (1 ~ 2 ~ 3), '123', '~ stays list-associative';
