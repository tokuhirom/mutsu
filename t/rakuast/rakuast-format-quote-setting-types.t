use v6.e.PREVIEW;
use Test;

sub expression($source) { $source.AST.statements[0].expression }

for <Format Formatter Formatter::Syntax> -> $name {
    my $node = expression($name);
    is $node.^name, 'RakuAST::Type::Simple', "$name is a setting type";
    is EVAL($node).^name, $name, "$name survives lowering";
}

for Q[q:o/%5s/], Q[q:format/%5s/], Q[qq:o/%5s/],
    Q[q:w:o/%5s/], Q[q:ww:format/%5s/], Q[q:o:!format/%5s/],
    Q[q:format:!o/%5s/], Q[q:o:o/%5s/] -> $source {
    my $node = expression($source);
    is $node.^name, 'RakuAST::QuotedString', "$source is a quoted string";
    is-deeply $node.processors, ('format',), "$source keeps the format processor";
    my $format = EVAL($node);
    isa-ok $format, Format, "$source produces a Format";
    is $format('x'), '    x', "$source uses the format compiler";
}

for Q[q:o:w/%s %s/], Q[q:o:ww/%s %s/] -> $source {
    my $node = expression($source);
    is-deeply $node.processors, ('format', $source.contains(':ww/') ?? 'quotewords' !! 'words'),
        "$source retains processor order";
    throws-like { EVAL($node) }, X::TypeCheck::Binding::Parameter,
        "$source passes a word list to the format processor";
}

my $spec = '5s';
my $interpolated = Q[qq:format/%$spec/].AST.statements[0].expression;
is $interpolated.segments.elems, 2, 'format quote retains interpolation segments';
is $interpolated.segments[1].^name, 'RakuAST::Var::Lexical', 'variable remains a lexical node';
my $format = EVAL($interpolated);
is $format(7), '    7', 'interpolated format executes after lowering';

my $constructed = RakuAST::QuotedString.new(
    processors => ('format',),
    segments => (RakuAST::StrLiteral.new('%3s:%2s'),),
);
my $callable = EVAL($constructed);
isa-ok $callable, Format, 'constructed format processor produces a Format';
is $callable('a', 'b'), '  a: b', 'constructed processor uses the same formatter';
is expression(Q[q:!format/%s/]).processors.elems, 0, 'negating a processor before installing it leaves plain text';

for <s c o O u> -> $symbol {
    is Formatter::Syntax.parse('%' ~ $symbol)<statement>[0]<directive><sym>.Str,
        $symbol, "the formatter grammar captures $symbol";
}

is qw/one/.^name, 'List', 'one plain quoted word remains a List';
is qww/one/.^name, 'List', 'one protected quoted word remains a List';
is <one>.^name, 'Str', 'one angle word retains its scalar form';
my $one = 'one';
is qqw/$one/.^name, 'List', 'one interpolated plain word remains a List';
is EVAL(Q[qw/one/].AST).^name, 'List', 'a plain word List survives the boundary';
is EVAL(Q[qww/one/].AST).^name, 'List', 'a protected word List survives the boundary';

done-testing;
