use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

# RakuAST Phase 4 slice 7 (ADR-0011): parameter-less Sub construction.

plan 9;

my $statements = RakuAST::StatementList.new;
$statements.add-statement(
    RakuAST::Statement::Expression.new(
        expression => RakuAST::IntLiteral.new(42),
    )
);
my $body = RakuAST::Blockoid.new($statements);

my $anonymous = RakuAST::Sub.new(body => $body);
isa-ok $anonymous, RakuAST::Sub, 'Sub.new constructs an anonymous Sub';
is $anonymous.body, $body, 'anonymous Sub exposes its body';
is $anonymous.gist, q:to/END/.chomp, 'anonymous Sub renders its body';
    RakuAST::Sub.new(
      body => RakuAST::Blockoid.new(
        RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::IntLiteral.new(42)
          )
        )
      )
    )
    END

my $empty = RakuAST::Sub.new;
isa-ok $empty.body, RakuAST::Blockoid, 'omitted body defaults to a Blockoid';
is $empty.body.statement-list.statements.elems, 0,
    'omitted body defaults to an empty StatementList';

my $named = RakuAST::Sub.new(
    name => RakuAST::Name.from-identifier('answer'),
    body => $body,
);
is $named.name.gist, RakuAST::Name.from-identifier('answer').gist,
    'named Sub exposes its name';
is $named.gist, q:to/END/.chomp, 'named Sub renders its name before its body';
    RakuAST::Sub.new(
      name => RakuAST::Name.from-identifier("answer"),
      body => RakuAST::Blockoid.new(
        RakuAST::StatementList.new(
          RakuAST::Statement::Expression.new(
            expression => RakuAST::IntLiteral.new(42)
          )
        )
      )
    )
    END

my @methods = RakuAST::Sub.^methods(:local)>>.name;
ok 'new' (elem) @methods && 'name' (elem) @methods && 'body' (elem) @methods,
    'Sub introspection exposes its constructor and accessors';

lives-ok { EVAL($named) }, 'constructed named Sub lowers through EVAL';
