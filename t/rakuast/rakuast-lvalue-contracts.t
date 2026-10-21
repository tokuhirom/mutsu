use v6.e.PREVIEW;
use experimental :rakuast;
use Test;

# A statement modifier inside a compound-assignment target must retain the
# assignment expression's container. The untaken modifier is a no-op.
is Q[my $s = "p"; ($s = "a" if 1) ~= "y"; $s].AST.EVAL, 'ay',
    'a taken modifier assignment remains a compound-assignment lvalue';
is Q[my $s = "p"; ($s = "a" if 0) ~= "y"; $s].AST.EVAL, 'p',
    'an untaken modifier assignment does not write';

# Type operands resolve through the term namespace, even when an EVAL unit
# cannot see the caller's lexical declaration while it compiles.
throws-like { EVAL(Q[Any.push(1)].AST) }, X::Assignment::RO,
    'a core type receiver has no writable container';
{
    my constant Alias = Any;
    throws-like { EVAL(Q[Alias.push(1)].AST) }, X::Assignment::RO,
        'a lexical type alias remains immutable across EVAL';
}
{
    my constant Any = Int;
    is EVAL(Q[Any].AST), Int, 'a lexical term alias shadows the setting type';
    my $node = RakuAST::Type::Simple.new(RakuAST::Name.from-identifier('Any'));
    is EVAL($node), Int, 'a constructed type reference sees the lexical alias';
}
{
    my \Any = [1];
    EVAL(Q[Any.push(2)].AST);
    is-deeply Any, [1, 2], 'a sigilless binding with the type spelling still mutates';
}
{
    my $Any = Any;
    EVAL(Q[$Any.push(2)].AST);
    is-deeply $Any, [2], 'a scalar with the type spelling still autovivifies';
}

# Even the invalid multidimensional combination has a public postfix tree:
# lowering it must retain the ordered colonpairs and raise X::Adverb.
my $bad = Q[my @a; @a[0;0;0]:!exists:delete].AST.statements[1].expression.postfix;
is $bad.colonpairs.map(*.^name).join(','),
    'RakuAST::ColonPair::False,RakuAST::ColonPair::True',
    'the invalid source form retains both ordered colonpairs';
is $bad.index.statements.elems, 3, 'the invalid source form retains all dimensions';
my $reversed = Q[my @a; @a[0;0;0]:delete:!exists].AST.statements[1].expression.postfix;
is $reversed.colonpairs.map(*.key).join(','), 'delete,exists',
    'the reversed invalid source form retains its written order';
throws-like {
    EVAL(Q[my @a = [[[42, 666, 314],],]; @a[0;0;0]:!exists:delete].AST)
}, X::Adverb, 'the invalid source form raises its adverb exception';
throws-like {
    EVAL(Q[my @a = [[[42, 666, 314],],]; @a[0;0;0]:delete:!exists].AST)
}, X::Adverb, 'the reversed invalid source form raises the same exception';

my $index = RakuAST::Postcircumfix::ArrayIndex.new(
    index => RakuAST::SemiList.new(
        RakuAST::Statement::Expression.new(expression => RakuAST::IntLiteral.new(0)),
        RakuAST::Statement::Expression.new(expression => RakuAST::IntLiteral.new(0)),
        RakuAST::Statement::Expression.new(expression => RakuAST::IntLiteral.new(0))),
    colonpairs => (
        RakuAST::ColonPair::False.new('exists'),
        RakuAST::ColonPair::True.new('delete')));
my $source = Q[my @a = [[[42, 666, 314],],]].AST.statements[0];
my $constructed = RakuAST::StatementList.new(
    $source,
    RakuAST::Statement::Expression.new(expression => RakuAST::ApplyPostfix.new(
        operand => RakuAST::Var::Lexical.new('@a'), postfix => $index)));
throws-like { EVAL($constructed) }, X::Adverb,
    'a constructed invalid multidimensional subscript raises the same exception';

done-testing;
