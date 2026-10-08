use Test;

# `:delete` on a multi-dimensional subscript in RakuAST, measured on rakudo
# 2026.09: the adverb is a colonpair of the postcircumfix, in source order,
# whatever builtin the parser lowers it to. EVAL of the tree deletes through
# the variable as the parsed program does.

plan 12;

my $plain = Q[my @a; @a[0;1]:delete].AST.statements[1].expression;
isa-ok $plain.postfix, RakuAST::Postcircumfix::ArrayIndex, 'a multi-dim :delete is an ArrayIndex';
is $plain.postfix.colonpairs.elems, 1, 'with one colonpair';
isa-ok $plain.postfix.colonpairs[0], RakuAST::ColonPair::True, 'a True colonpair';
is $plain.postfix.colonpairs[0].key, 'delete', 'named delete';
is $plain.postfix.index.statements.elems, 2, 'over two dimensions';

my $cond = Q[my @a; my $c; @a[0;1]:delete($c)].AST.statements[2].expression;
isa-ok $cond.postfix.colonpairs[0], RakuAST::ColonPair::Value, ':delete(COND) is a Value colonpair';

my $both = Q[my @a; @a[0;1]:k:delete].AST.statements[1].expression;
is $both.postfix.colonpairs.map(*.key).join(','), 'k,delete', 'a value adverb keeps its order';

my $exists = Q[my @a; @a[0;1]:exists:delete].AST.statements[1].expression;
is $exists.postfix.colonpairs.map(*.key).join(','), 'exists,delete', ':exists:delete';

sub run($src) { EVAL($src.AST) }
is run(Q[my @a[2;2] = (1, 2), (3, 4); @a[0;1]:delete]), 2, 'the deleted element is returned';
is run(Q[my @a[2;2] = (1, 2), (3, 4); @a[1;0]:delete; @a[1;0]:exists]), False, 'and removed';
is run(Q[my @a[2;2] = (1, 2), (3, 4); my $c = 0; @a[0;0]:delete($c); @a[0;0]]), 1,
    ':delete(False) keeps the element';
is run(Q[my @a[2;2] = (1, 2), (3, 4); @a[1;1]:k:delete.join(",")]), '1,1', 'a key adverb reads the removed element';
