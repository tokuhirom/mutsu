use Test;

# raku drops ONE level of parentheses around a postfix operand, a parenthesized
# pair included: `(a => 1).List` is `ApplyPostfix(operand => FatArrow ...)`,
# while `((a => 1)).List` keeps the inner `Circumfix::Parentheses`. Checked
# against `raku` first; this file must pass under both `raku` and `mutsu`.

plan 8;

sub count-parens(Str $src) { $src.AST.gist.comb('Circumfix::Parentheses').elems }

is count-parens(Q{(a => 1).List}), 0, 'bareword pair: the one level is dropped';
is count-parens(Q{(a => 1)<x>}), 0, 'also before a subscript';
is count-parens(Q{((a => 1)).List}), 1, 'a second level is kept';
is count-parens(Q{(a => 1)}), 1, 'a bare parenthesized pair keeps its parentheses';
is count-parens(Q{("a" => 1).List}), 0, 'quoted-key pair: the one level is dropped';
ok Q{(a => 1).List}.AST.gist.contains('FatArrow'), 'the operand is the FatArrow';

my $ast = Q{(a => 1).List}.AST;
is-deeply EVAL($ast), (:a(1),).List, 'the round trip still evaluates';
is-deeply EVAL(Q{((a => 1)).List}.AST), (:a(1),).List, 'two levels evaluate too';
