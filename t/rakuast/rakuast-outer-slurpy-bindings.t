use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

plan 27;

sub statements($node) { $node.body.statement-list.statements }
sub expression($source) { $source.AST.statements[0].expression }

is expression(Q[-> *%_ { %_.elems }]).^name, 'RakuAST::PointyBlock',
    'an explicit named-slurpy pointy block retains its signature';
is expression(Q[-> *@_ { @_.elems }]).^name, 'RakuAST::PointyBlock',
    'an explicit positional-slurpy pointy block retains its signature';
is expression(Q[my &f = { @_.elems + %_.elems }]).initializer.expression.^name,
    'RakuAST::Block', 'mixed aggregate placeholders retain a bare Block';
is EVAL(Q[my &f = { @_.elems + %_.elems }; f(1, 2, :a)].AST), 3,
    'a source block binds positional and named aggregates independently';
is EVAL(Q[my &f = { $^x + %_.elems }; f(2, :a)].AST), 3,
    'a scalar placeholder and named aggregate share one implicit signature';
ok EVAL(Q[/<word(:expected{ @_.elems + %_.elems })>/].AST) ~~ Regex,
    'a mixed placeholder block survives regex argument source reconstruction';

my $hash = expression(Q[sub f(*%_) { { %_.elems } }]);
is statements(statements($hash)[0].expression)[0].expression.operand.^name,
    'RakuAST::Var::Lexical', 'a nested block reads the enclosing named slurpy';
my $array = expression(Q[sub f(*@_) { { @_.elems } }]);
is statements(statements($array)[0].expression)[0].expression.operand.^name,
    'RakuAST::Var::Lexical', 'a nested block reads the enclosing positional slurpy';

my $sum = statements(expression(Q[{ %_.elems + %_.elems }]))[0].expression;
is $sum.left.operand.^name, 'RakuAST::VarDeclaration::Placeholder::SlurpyHash',
    'the first unbound reference declares the placeholder';
is $sum.right.operand.^name, 'RakuAST::Var::Lexical',
    'the later reference uses that binding';
my @after = statements(expression(Q[{ %_.elems; { %_.elems } }]));
is statements(@after[1].expression)[0].expression.operand.^name,
    'RakuAST::Var::Lexical', 'a later nested block sees the placeholder binding';
my @before = statements(expression(Q[{ { %_.elems }; %_.elems }]));
is statements(@before[0].expression)[0].expression.operand.^name,
    'RakuAST::VarDeclaration::Placeholder::SlurpyHash',
    'an earlier nested block owns its first unbound reference';
is @before[1].expression.operand.^name,
    'RakuAST::VarDeclaration::Placeholder::SlurpyHash',
    'a nested declaration does not leak into its parent';

is EVAL(Q[sub f(*%_) { (1, 2).map({ $_ + %_.elems }).join(',') }; f(:a, :b)].AST),
    '3,4', 'a mapped block captures the named slurpy and keeps its topic';
is EVAL(Q[sub f(*@_) { (1, 2).map({ $_ + @_.elems }).join(',') }; f(1, 2, 3)].AST),
    '4,5', 'a mapped block captures the positional slurpy';
is EVAL(Q[my &f = sub (*%_) { { %_.elems }() }; f(:a)].AST), 1,
    'an anonymous routine supplies the lexical binding';
is EVAL(Q[class C { method f { { %_.elems }() } }; C.f(:a, :b)].AST), 2,
    'a method supplies its implicit named slurpy';
is EVAL(Q[my &f = -> *%_ { { %_.elems }() }; f(:a, :b)].AST), 2,
    'a pointy block supplies its explicit named slurpy';
is EVAL(Q[class PointyOuter { method m(*%_) { my &f = -> *%_ { (1, 2).map({ $_ + %_.elems }).join(',') }; f(:inner) } }; PointyOuter.m(:a, :b)].AST),
    '2,3', 'an explicit pointy named slurpy shadows the method binding';
is EVAL(Q[sub outer(*@_) { my &f = -> *@_ { (1, 2).map({ $_ + @_.elems }).join(',') }; f(4, 5, 6) }; outer(9)].AST),
    '4,5', 'an explicit pointy positional slurpy shadows the routine binding';
is EVAL(Q[my &f = { %_.elems; { %_.elems }() }; f(:a, :b)].AST), 2,
    'an implicit placeholder binding reaches a nested block';
my $declared = Q[my %_; { %_.elems }].AST.statements[1].expression;
is statements($declared)[0].expression.operand.^name,
    'RakuAST::Var::Lexical', 'an explicit lexical declaration reaches a nested block';
is EVAL(Q[class CLiteral { }; my &m = method (@a, *%_) { @a.map({ $_ ~ %_.elems }).join(',') }; CLiteral.&m(<a b>, :x)].AST),
    'a1,b1', 'a method literal supplies its explicit named slurpy';
my $literal = expression(Q[method { { %_.elems } }]);
is statements(statements($literal)[0].expression)[0].expression.operand.^name,
    'RakuAST::VarDeclaration::Placeholder::SlurpyHash',
    'a method literal retains Rakudo\'s unresolved implicit named read';

sub block($expr) {
    my $block = RakuAST::Block.new(may-have-signature => True, body => RakuAST::Blockoid.new(
        RakuAST::StatementList.new(RakuAST::Statement::Expression.new(expression => $expr))));
    RakuAST::StatementList.new(
        RakuAST::Statement::Expression.new(expression => RakuAST::VarDeclaration::Simple.new(
            sigil => '$', desigilname => RakuAST::Name.from-identifier('callable'),
            initializer => RakuAST::Initializer::Assign.new($block))),
        RakuAST::Statement::Expression.new(expression => RakuAST::Var::Lexical.new('$callable')));
}
my &named = EVAL(block(RakuAST::VarDeclaration::Placeholder::SlurpyHash.new));
is named(:a, :b).elems, 2, 'a constructed hash placeholder declares a local parameter';
my &positional = EVAL(block(RakuAST::VarDeclaration::Placeholder::SlurpyArray.new));
is positional(1, 2, 3).elems, 3, 'a constructed array placeholder declares a local parameter';
my &both = EVAL(block(RakuAST::ApplyInfix.new(
    left => RakuAST::ApplyPostfix.new(
        operand => RakuAST::VarDeclaration::Placeholder::SlurpyArray.new,
        postfix => RakuAST::Call::Method.new(name => RakuAST::Name.from-identifier('elems'))),
    infix => RakuAST::Infix.new('+'),
    right => RakuAST::ApplyPostfix.new(
        operand => RakuAST::VarDeclaration::Placeholder::SlurpyHash.new,
        postfix => RakuAST::Call::Method.new(name => RakuAST::Name.from-identifier('elems'))))));
is both(1, 2, :a), 3, 'constructed positional and named placeholders bind independently';
