use Test;
use experimental :rakuast;

plan 13;

my $angle = Q[<< a <<b>> c >>].AST.statements[0].expression;
is $angle.^name, 'RakuAST::QuotedString', 'a doubled angle quote is a word quote';
is $angle.segments.map(*.^name).join('|'),
    'RakuAST::StrLiteral|RakuAST::QuotedString|RakuAST::StrLiteral',
    'its nested delimiter creates a child word quote';
is EVAL($angle).join('|'), 'a|<<|b|>>|c', 'the source tree evaluates through the nested words';

my $french = Q[« a «b» c »].AST.statements[0].expression;
is $french.segments[1].^name, 'RakuAST::QuotedString',
    'a nested French quote has the same node shape';
is EVAL($french).join('|'), 'a|«|b|»|c', 'the French delimiter survives lowering';

my $constructed = RakuAST::QuotedString.new(
    processors => <quotewords val>,
    segments => (
        RakuAST::StrLiteral.new(' a <<'),
        RakuAST::QuotedString.new(
            processors => <quotewords val>,
            segments => (RakuAST::StrLiteral.new('b'),),
        ),
        RakuAST::StrLiteral.new('>> c '),
    ),
);
is EVAL($constructed).join('|'), 'a|<<|b|>>|c',
    'a constructed nested word quote uses the same lowering';

is Q[<< a «b» >>].AST.statements[0].expression.segments.elems, 1,
    'a different delimiter remains ordinary word text';
is Q[« a <<b>> »].AST.statements[0].expression.segments.elems, 1,
    'the delimiter distinction works in both directions';

my $inline = Q[use Test "ignored", proto sub helper(|) is export {*};].AST.statements[0];
is $inline.^name, 'RakuAST::Statement::Use', 'the inline proto is one use statement';
is $inline.argument.operands[1].^name, 'RakuAST::Sub',
    'the proto is the second use argument';
is $inline.argument.operands[1].multiness, 'proto',
    'the argument retains its proto declaration';

my $traced = Q[use trace; say 1].AST;
is $traced.statements.elems, 2, 'trace hooks stay private to the statement nodes';

my $proc = run(
    $*EXECUTABLE, '-I', 't/lib', '-e',
    q[use experimental :rakuast; EVAL(Q[use UseInlineProtoArg "ignored", proto sub helper(|) is export {*};].AST); say "loaded"],
    :out, :err,
);
my $out = $proc.out.slurp(:close).trim;
my $err = $proc.err.slurp(:close);
is-deeply ($proc.exitcode, $out), (0, "Sub\nloaded"),
    'EVAL of an inline proto use node passes the proto object to EXPORT'
    or diag $err;
