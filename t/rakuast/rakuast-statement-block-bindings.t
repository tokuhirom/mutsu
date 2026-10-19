use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

plan 39;

sub block($expression, :$signature = True) {
    RakuAST::Block.new(may-have-signature => $signature,
        body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(:$expression))))
}
sub statement($expression) { RakuAST::Statement::Expression.new(:$expression) }
sub placeholder($type) {
    $type eq 'Array' ?? RakuAST::VarDeclaration::Placeholder::SlurpyArray.new
        !! RakuAST::VarDeclaration::Placeholder::SlurpyHash.new
}
ok block(RakuAST::IntLiteral.new(7)).may-have-signature,
    'the constructor preserves the true signature capability';
nok RakuAST::Block.new.may-have-signature,
    'an empty constructed block defaults to no signature capability';
nok block(RakuAST::IntLiteral.new(7), :!signature).gist.contains('may-have-signature'),
    'the constructor omits a false signature capability';
dies-ok { EVAL(block(RakuAST::VarDeclaration::Placeholder::Positional.new('$x'), :!signature)) },
    'a signature-free root block rejects a positional placeholder';
dies-ok { EVAL(statement(block(RakuAST::VarDeclaration::Placeholder::Positional.new('$x'), :!signature))) },
    'a signature-free statement block rejects a positional placeholder';

for 'Array', 'Hash' -> $type {
    is EVAL(block(placeholder($type))).^name, $type, 'a root block executes its aggregate placeholder';
    is EVAL(block(placeholder($type))).elems, 0, 'a root aggregate receives no arguments';
    is EVAL(statement(block(placeholder($type)))).^name, $type, 'a wrapped statement block executes';
    is EVAL(statement(block(placeholder($type)))).elems, 0, 'a wrapped aggregate receives no arguments';
    dies-ok { EVAL(block(placeholder($type), :!signature)) },
        'a block that cannot take a signature rejects an aggregate placeholder';
    dies-ok { EVAL(statement(block(placeholder($type), :!signature))) },
        'the statement wrapper preserves the signature restriction';
}

sub caller(*@_, *%_) {
    is EVAL(block(RakuAST::Var::Lexical.new('@_'), :!signature)).elems, 2,
        'a block without a signature may read an explicit outer positional lexical';
    is EVAL(block(RakuAST::Var::Lexical.new('%_'), :!signature)).elems, 1,
        'a block without a signature may read an explicit outer named lexical';
    for 'Array', 'Hash' -> $type {
        is EVAL(block(placeholder($type))).^name, $type, 'a caller does not change the root result type';
        is EVAL(block(placeholder($type))).elems, 0, 'a root block does not borrow caller arguments';
        is EVAL(statement(block(placeholder($type)))).^name, $type, 'a caller does not change the wrapped result';
        is EVAL(statement(block(placeholder($type)))).elems, 0, 'a wrapped block does not borrow caller arguments';
    }
}
caller(1, 2, :a);

dies-ok { EVAL(block(RakuAST::VarDeclaration::Placeholder::Positional.new('$x'))) },
    'a root positional placeholder is missing its argument';
dies-ok { EVAL(statement(block(RakuAST::VarDeclaration::Placeholder::Positional.new('$x')))) },
    'a wrapped positional placeholder is missing its argument';
is EVAL(block(RakuAST::IntLiteral.new(7))), 7, 'a root block without parameters still executes';
is EVAL(statement(block(RakuAST::IntLiteral.new(7)))), 7, 'an ordinary statement block still executes';

is EVAL(Q[my &f = { @_.elems }; f(1, 2)].AST), 2, 'an initializer block remains callable';
is EVAL(Q[my &f = { %_.elems }; f(:a, :b)].AST), 2, 'a named initializer block remains callable';
is EVAL(Q[sub f(*@_) { { @_.elems } }; f(1, 2, 3)].AST), 3,
    'an explicit enclosing positional binding remains lexical';
is EVAL(Q[sub f(*%_) { { %_.elems } }; f(:a, :b)].AST), 2,
    'an explicit enclosing named binding remains lexical';

sub callable($placeholder) {
    EVAL(RakuAST::StatementList.new(
        statement(RakuAST::VarDeclaration::Simple.new(
            sigil => '$', desigilname => RakuAST::Name.from-identifier('callable'),
            initializer => RakuAST::Initializer::Assign.new(block($placeholder)))),
        statement(RakuAST::Var::Lexical.new('$callable'))))
}
my &positional_callable = callable(RakuAST::VarDeclaration::Placeholder::SlurpyArray.new);
is positional_callable(1, 2).elems, 2, 'a constructed expression block binds supplied positional arguments';
my &named_callable = callable(RakuAST::VarDeclaration::Placeholder::SlurpyHash.new);
is named_callable(:a, :b).elems, 2, 'a constructed expression block binds supplied named arguments';
is EVAL(Q[sub f(*@_) { { my @_ = (); @_ }; @_.elems }; f(1, 2)].AST), 2,
    'an inner aggregate declaration leaves the outer positional binding intact';
is EVAL(Q[sub f(*%_) { { my %_ = (); %_ }; %_.elems }; f(:a, :b)].AST), 2,
    'an inner aggregate declaration leaves the outer named binding intact';
