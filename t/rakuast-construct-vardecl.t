use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

# RakuAST Phase 4 slice 9: construction of plain variable declarations.

plan 13;

my $name = RakuAST::Name.from-identifier('answer');
my $declaration = RakuAST::VarDeclaration::Simple.new(
    sigil => '$',
    desigilname => $name,
);
isa-ok $declaration, RakuAST::VarDeclaration::Simple,
    'VarDeclaration::Simple.new constructs a declaration';
is $declaration.sigil, '$', 'the declaration exposes its sigil';
is $declaration.desigilname, $name, 'the declaration exposes its name';
is $declaration.gist, q:to/END/.chomp, 'an uninitialized declaration renders like Rakudo';
    RakuAST::VarDeclaration::Simple.new(
      sigil       => "\$",
      desigilname => RakuAST::Name.from-identifier("answer")
    )
    END

my $initializer = RakuAST::Initializer::Assign.new(
    RakuAST::IntLiteral.new(42),
);
isa-ok $initializer, RakuAST::Initializer::Assign,
    'Initializer::Assign.new constructs an initializer';
is $initializer.expression.value, 42, 'the initializer exposes its expression';

my $initialized = RakuAST::VarDeclaration::Simple.new(
    sigil => '$',
    desigilname => $name,
    initializer => $initializer,
);
is $initialized.initializer, $initializer,
    'the declaration exposes its initializer';
is $initialized.gist, q:to/END/.chomp, 'an initialized declaration renders like Rakudo';
    RakuAST::VarDeclaration::Simple.new(
      sigil       => "\$",
      desigilname => RakuAST::Name.from-identifier("answer"),
      initializer => RakuAST::Initializer::Assign.new(
        RakuAST::IntLiteral.new(42)
      )
    )
    END

my @declaration-methods = RakuAST::VarDeclaration::Simple.^methods(:local)>>.name;
ok 'new' (elem) @declaration-methods
        && 'sigil' (elem) @declaration-methods
        && 'desigilname' (elem) @declaration-methods
        && 'initializer' (elem) @declaration-methods,
    'declaration introspection exposes its constructor and accessors';
my $statements = RakuAST::StatementList.new;
$statements.add-statement(
    RakuAST::Statement::Expression.new(expression => $initialized),
);
$statements.add-statement(
    RakuAST::Statement::Expression.new(
        expression => RakuAST::Var::Lexical.new('$answer'),
    ),
);
is EVAL($statements), 42, 'the constructed declaration lowers through EVAL';

lives-ok {
    RakuAST::VarDeclaration::Simple.new(
        sigil => '@',
        desigilname => RakuAST::Name.from-identifier('items'),
    )
}, 'non-scalar sigils are accepted';
dies-ok {
    RakuAST::VarDeclaration::Simple.new(sigil => '$')
}, 'a desigilname is required';
dies-ok {
    RakuAST::VarDeclaration::Simple.new(
        sigil => '$',
        desigilname => RakuAST::IntLiteral.new(1),
    )
}, 'desigilname must be a Name node';
