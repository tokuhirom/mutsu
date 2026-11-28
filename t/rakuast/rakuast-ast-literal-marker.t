use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

plan 5;

my $marker = "\x[10FFFD]";
my $source = '"' ~ $marker ~ 'xFF"';
my $ast = $source.AST;
my $literal = $ast.statements[0].expression.segments[0].value;

is $literal.chars, 4, 'Str.AST exposes one literal marker in its string leaf';
is-deeply $literal.ords.List, ($marker ~ 'xFF').ords.List,
    'the AST leaf has the codepoints of the source string';
is EVAL($ast), $marker ~ 'xFF', 'AST EVAL preserves the literal marker';
is EVAL($ast).chars, 4, 'AST EVAL does not synthesize an invalid byte';
is EVAL($source).chars, 4, 'string EVAL parses the literal marker once';
