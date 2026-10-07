use v6;
use experimental :rakuast;
use Test;

# `do whenever ...` as an expression is `StatementPrefix::Do` over a
# `Statement::Whenever` (measured on rakudo 2026.09) and EVALs back to the
# whenever's tap.

plan 4;

my $src = Q|my $kind; react { my $tap = do whenever Supplier.new.Supply -> $v { }; $kind = $tap.^name; done }; $kind|;
my $ast = $src.AST;
my $react = $ast.statements[1].expression;
is $react.^name, 'RakuAST::StatementPrefix::React', 'react wraps the block';
my $decl = $react.blorst.body.statement-list.statements[0].expression;
my $do = $decl.initializer.expression;
is $do.^name, 'RakuAST::StatementPrefix::Do', 'the initializer is a Do prefix';
is $do.blorst.^name, 'RakuAST::Statement::Whenever', 'over a whenever statement';
is $ast.EVAL, 'Tap', 'and the tree EVALs to the tap the whenever yields';
