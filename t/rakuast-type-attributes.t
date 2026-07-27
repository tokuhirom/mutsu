use Test;
use experimental :rakuast;

plan 11;

my @attrs = RakuAST::ApplyInfix.^attributes(:local);
is @attrs.map(*.name).sort.join(','), '$!infix,$!left,$!right',
    'ApplyInfix exposes its three model fields';
is @attrs.map(*.^name).unique.join(','), 'Attribute',
    'RakuAST fields use ordinary Attribute introspection objects';
is @attrs[0].package.^name, 'RakuAST::ApplyInfix',
    'attribute package identifies the declaring RakuAST class';
is @attrs[0].type.^name, 'Mu', 'model fields have an unconstrained type';
ok !@attrs[0].has_accessor, 'model storage remains private';

my $node = RakuAST::ApplyInfix.new(
    left  => RakuAST::IntLiteral.new(1),
    infix => RakuAST::Infix.new('+'),
    right => RakuAST::IntLiteral.new(2),
);
is $node.^attributes(:local).map(*.name).sort.join(','), '$!infix,$!left,$!right',
    'node values share their type object attribute metadata';

is RakuAST::IntLiteral.^attributes(:local).map(*.name).join(','),
    '$!value', 'literal value is discoverable as a model field';
is RakuAST::Var::Lexical.^attributes(:local).map(*.name).join(','),
    '$!name', 'lexical variable name is discoverable as a model field';
is RakuAST::StatementList.^attributes(:local).map(*.name).join(','),
    '$!statements', 'statement children are discoverable as a model field';
is RakuAST::Statement::Expression.^attributes(:local).map(*.name).join(','),
    '$!expression,$!loop-modifier',
    'statement expression and modifier are discoverable as model fields';
is RakuAST::Assignment.^attributes(:local).elems, 0,
    'a class without modeled fields reports no local attributes';
