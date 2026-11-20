use experimental :rakuast;
use Test;

# `Str.AST` parses a unit numbered like an `EVAL`'s, so `.fileline` names it
# (`EVAL_3:2`) and `Origin::Source.original-file` answers it;
# `StatementList.add-doc-block` / `insert-doc-block` put a Pod block among
# the statements, and `code-statements` leaves Pod blocks out (#14916,
# measured on rakudo 2026.11).

plan 12;

my $before = EVAL q[$?FILE];
ok $before ~~ /^ 'EVAL_' (\d+) $/, 'an EVAL unit is numbered';
my $n = +$0;

my $ast = Q[say 1;
say 2].AST;
is $ast.fileline, "EVAL_{$n + 1}:1", 'a Str.AST unit takes the next EVAL number';
is $ast.statements[1].fileline, "EVAL_{$n + 1}:2", 'a statement names its unit and line';
is $ast.statements[0].expression.args.fileline, "EVAL_{$n + 1}:1", 'so does a node below it';
is $ast.statements[1].origin.source.original-file, "EVAL_{$n + 1}",
    'Origin::Source.original-file';
is EVAL(q[$?FILE]), "EVAL_{$n + 2}", 'EVAL and Str.AST share the counter';

my $list = Q[say 1; say 2].AST;
my $doc = RakuAST::Doc::Block.new(type => 'pod', paragraphs => ("x\n",));
isa-ok $list.add-doc-block($doc), RakuAST::Doc::Block, 'add-doc-block answers the block';
is $list.statements.map(*.^name).join(' '),
    'RakuAST::Statement::Expression RakuAST::Statement::Expression RakuAST::Doc::Block',
    'and puts it after the statements';
$list.insert-doc-block(1, $doc);
is $list.statements.map(*.^name).join(' '),
    'RakuAST::Statement::Expression RakuAST::Doc::Block RakuAST::Statement::Expression RakuAST::Doc::Block',
    'insert-doc-block puts it before the statement at the index';
is $list.code-statements.map(*.^name).join(' '),
    'RakuAST::Statement::Expression RakuAST::Statement::Expression',
    'code-statements leaves the Pod blocks out';
is Q[=begin pod
x
=end pod
say 1].AST.code-statements.map(*.^name).join(' '), 'RakuAST::Statement::Expression',
    'a parsed Pod block too';
is $list.DEPARSE, "say 1;\n=begin pod\n\nx\n=end pod\n\nsay 2\n=begin pod\n\nx\n=end pod\n\n",
    'the blocks deparse among the statements';
