use Test;

# A statement's source line survives the RakuAST round trip. The tree keeps it
# as a hidden `origin` on the statement node (rakudo keeps an origin on every
# node, and its `.raku` shows none either), and lowering puts the line marker
# back, so a tree read from source reports the lines its source does.

plan 10;

sub line-of($e) { $e.backtrace.map(*.line).first(* > 0) }

my $src = "my \$x = 1;\n\ndie 'boom'";

try EVAL($src);
my $string-line = line-of($!);
try EVAL($src.AST);
is line-of($!), $string-line, 'EVAL of a parsed tree fails on the same line as its source';
like $!.gist, / 'line 3' /, 'and that is the line the statement was written on';

# The origin is not part of the model's shape.
nok $src.AST.gist.contains('origin'), 'the rendered node shows no origin';
is $src.AST.statements.elems, 2, 'the origin adds no statement';
isa-ok $src.AST.statements[1], RakuAST::Statement::Expression, 'a statement is still a statement';

# A call reports the caller's line, as the source does.
my $prog = "sub where-from() \{ callframe(1).line \}\n\nwhere-from()";
is EVAL($prog.AST), 3, 'a zero-argument call carries the line of its call site';
is EVAL($prog.AST), EVAL($prog), 'the same line the source reports';

# A hand-built tree has no origin and lowers as it always did.
is EVAL(RakuAST::StatementList.new(
    RakuAST::Statement::Expression.new(expression => RakuAST::IntLiteral.new(7))
)), 7, 'a hand-built tree needs no origin';
is EVAL(RakuAST::StatementList.new(
    RakuAST::Statement::Expression.new(
        expression => RakuAST::Call::Name.new(name => RakuAST::Name.from-identifier('abs'),
            args => RakuAST::ArgList.new(RakuAST::IntLiteral.new(-3))))
)), 3, 'a hand-built call lowers without a call-site line';

# A statement later in a unit keeps its own line.
my $two = "my \$a = 1;\n\n\nmy \$b = 2;\nsub l() \{ callframe(1).line \}\nl()";
is EVAL($two.AST), 6, 'each statement keeps the line it began on';
