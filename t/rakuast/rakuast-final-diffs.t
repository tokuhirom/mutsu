use Test;
use experimental :rakuast;

plan 8;

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
