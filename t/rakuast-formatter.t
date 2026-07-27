use v6.e.PREVIEW;
use Test;
use experimental :rakuast;

plan 5;

my $ast = Formatter.AST('value=%04d');
ok $ast ~~ RakuAST::Node, 'Formatter.AST returns a genuine RakuAST node';
is $ast.^name, 'RakuAST::PointyBlock', 'the formatter AST is a callable pointy block';
is $ast.signature.parameters.elems, 1, 'the formatter AST has one slurpy parameter';
is $ast.signature.parameters[0].target.name, '@args',
    'the formatter AST accepts the format arguments';

my &formatter = EVAL $ast;
is formatter(23), 'value=0023', 'the formatter AST lowers to an executable callable';
