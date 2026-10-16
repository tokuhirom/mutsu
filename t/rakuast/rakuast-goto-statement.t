use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

plan 4;

# Rakudo models a labelled goto as a statement expression containing a
# normal listop call. The internal parser uses Stmt::Goto, so conversion and
# lowering must bridge those representations without changing the compiler.
my $ast = Q[FOO: goto FOO].AST;
my $statement = $ast.statements[0];
is $statement.^name, 'RakuAST::Statement::Expression',
    'a labelled goto is a statement expression';
ok $statement.expression.gist.contains('RakuAST::Call::Name::WithoutParentheses'),
    'goto uses the bare call node';
ok $statement.expression.gist.contains('name => RakuAST::Name.from-identifier("goto")'),
    'the call name is goto';
ok $statement.expression.gist.contains('RakuAST::Name.from-identifier("FOO")'),
    'the target label is a name argument';
