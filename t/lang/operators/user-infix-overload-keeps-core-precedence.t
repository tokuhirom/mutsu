use Test;

# A `multi infix:<~>` (or `&&`, `||`, `^^`, `//`, or a comparison: `==`, `!=`,
# `<`, `>`, `<=`, `>=`) with no precedence trait adds
# a candidate to the core operator, which keeps its own precedence: it is not a
# new operator at the trait-less additive level (Red exports `multi infix:<~>`,
# after which `'b' x 3 ~ 'a'` read as `'b' x (3 ~ 'a')`; it exports
# `multi infix:«<»` too, after which `3 < 1 + 3` was `(3 < 1) + 3`).

plan 15;

class P { }
multi infix:<~>(P $a, P $b) { 'user ~' }
multi infix:<&&>(P $a, P $b) { 'user &&' }
multi infix:<||>(P $a, P $b) { 'user ||' }
multi infix:<//>(P $a, P $b) { 'user //' }
multi infix:<==>(P $a, P $b) { 'user ==' }
multi infix:«<»(P $a, P $b) { 'user <' }
multi infix:<!=>(P $a, P $b) { 'user !=' }

is 'b' x 3 ~ 'a', 'bbba', '~ stays looser than x';
is 1 + 2 ~ 3 + 4, '37', '~ stays looser than +';
is P.new ~ P.new, 'user ~', 'the declared candidate is called';
is 1 + 2 && 3 + 4, 7, '&& stays looser than +';
is 0 + 0 || 3 + 4, 7, '|| stays looser than +';
is Int // 3 + 4, 7, '// stays looser than +';
is 2 x 2 && 3, 3, '&& stays looser than x';
is ('a' ~ 'b' x 2), 'abb', 'x binds tighter than ~ on its right';
is (1 ~ 2 ~ 3), '123', '~ stays list-associative';
is 1 + 1 == 2, True, '== stays looser than +';
is 3 < 1 + 3, True, '< stays looser than +';
is 5 - 1 != 4, False, '!= stays looser than -';
is P.new == P.new, 'user ==', 'the == candidate is called';
is P.new < P.new, 'user <', 'the < candidate is called';
is (1 < 2 < 3), True, '< still chains';
