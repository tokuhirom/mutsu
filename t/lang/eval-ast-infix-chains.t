use Test;
use experimental :rakuast;

plan 8;

# `EVAL($ast)` keeps what the parser's own tree does for an infix run: an
# imported list-associative infix takes the whole chain, a declared comparison
# word chains pair by pair, and a set-relation symbol is the set operator.

my $list = Q|use lib 't/lib'; use ListAssocInfixChain; (1 lsx 2 lsx 3), (1 l3x 2 l3x 3), (1 lsx 2)|.AST;
is-deeply EVAL($list).List, ("LS(1,2,3)", "L3(1,2,3)", "LS(1,2)"), 'imported list-assoc infixes take every operand';

my $chain = Q|class T { has $.v }
multi infix:<eq>(T \a, T \b) { a.v == b.v }
my ($x, $y, $z) = T.new(:v(1)), T.new(:v(1)), T.new(:v(2));
("a" eq "a" eq "a"), ("a" eq "b" eq "b"), ($x eq $y eq $y), ($x eq $y eq $z)|.AST;
is-deeply EVAL($chain).List, (True, False, True, False), 'a declared eq chains pair by pair';

my $set = Q|my @c[2;2] = [1,2],[3,4]; (3 ∈ @c), (5 ∈ @c), (@c ∋ 4), (@c ⊆ (1..4)), (@c ⊇ (1,)), ((1,) ⊂ @c), (@c ⊃ (1,))|.AST;
is-deeply EVAL($set).List, (True, False, True, True, True, True, True), 'set relation symbols flatten a shaped array';

is-deeply EVAL(Q|(3 ∈ (1, 2, 3)), (3 ∋ 3)|.AST).List, (True, True), 'the symbols on plain operands';
is-deeply EVAL(Q|(1 min 2 min 3), (1 max 2 max 3), (True xor False xor False)|.AST).List, (1, 3, True), 'core list infixes are unchanged';
is-deeply EVAL(Q|(1 < 2 < 3), (1 < 3 < 2)|.AST).List, (True, False), 'a core chain is unchanged';
is-deeply EVAL(Q|(1 ∈ (1,)), (2 ∈ (1,))|.AST).List, (True, False), 'membership through the symbol';
is-deeply EVAL(Q|(1 lt 2 lt 3), ("a" lt "b" lt "a")|.AST).List, (True, False), 'a word comparison chain';
