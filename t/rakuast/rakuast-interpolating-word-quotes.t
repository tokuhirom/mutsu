use Test;

# A `quotewords` quote that interpolates or quotes a word, measured on rakudo
# 2026.09: a `QuotedString` with `processors => <quotewords val>` whose segments
# are the text as written (unquoted runs, interpolated terms, and one
# `QuoteWordsAtom` per quoted word).

plan 16;

my $ast = Q[«a "b c"»].AST.statements.head.expression;
isa-ok $ast, RakuAST::QuotedString, '`«a "b c"»` is a QuotedString';
is $ast.processors.join(' '), 'quotewords val', 'with the quotewords and val processors';
is $ast.segments.map(*.^name).join(' '),
    'RakuAST::StrLiteral RakuAST::QuoteWordsAtom', 'a literal run and a quoted-word atom';
is $ast.segments[0].value, 'a ', 'the run keeps its trailing whitespace';
is $ast.segments[1].atom.segments.head.value, 'b c', 'the atom wraps the quoted text';

my $var = Q[qqww/a $b "c d"/].AST.statements.head.expression;
is $var.processors.join(' '), 'quotewords', 'qqww has only the quotewords processor';
is $var.segments.map(*.^name).join(' '),
    'RakuAST::StrLiteral RakuAST::Var::Lexical RakuAST::StrLiteral RakuAST::QuoteWordsAtom',
    'a variable is a segment between literal runs';

is Q[<<a $b c>>].AST.statements.head.expression.segments.elems, 3,
    '`<<a $b c>>` has three segments';

my $b = 'X';
is-deeply EVAL(Q[«a "b c"»].AST), EVAL(Q[«a "b c"»]), 'a quoted word survives the round trip';
is-deeply EVAL(Q[<<a $b>>].AST), EVAL(Q[<<a $b>>]), 'an interpolated word survives it';
is-deeply EVAL(Q[qqww/a $b "c d"/].AST), EVAL(Q[qqww/a $b "c d"/]), 'qqww survives it';
is-deeply EVAL(Q[<<a$b c>>].AST), EVAL(Q[<<a$b c>>]), 'an interpolation touching text stays one word';
is-deeply EVAL(Q[<<"x $b" y>>].AST), EVAL(Q[<<"x $b" y>>]), 'an interpolating quoted word survives it';
is-deeply EVAL(Q[qqww:v/1 $b/].AST), EVAL(Q[qqww:v/1 $b/]), '`:v` survives it';
is Q[qw:v/1 2/].AST.statements.head.expression.processors.join(' '), 'words val',
    '`qw:v` carries the val processor';
is-deeply EVAL(Q[qw:v/1 2/].AST), EVAL(Q[qw:v/1 2/]), 'and keeps its allomorphs';
