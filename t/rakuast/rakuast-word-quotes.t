use Test;

# Word quotes other than `<…>` in RakuAST, measured on rakudo 2026.09: one
# `QuotedString` whose `processors` name how the text is split (`words`, or
# `quotewords` for `qww`/`«…»`; `«…»` and `<<…>>` also say `val`) over the raw
# text as a single segment. Interpolating forms (`<<a $b>>`) are not covered.

plan 27;

for 'qw/a  b/', 'qw<a  b>', 'Qw/a  b/', 'q:w/a  b/' -> $src {
    my $q = "my @w = $src;".AST.statements.head.expression.initializer.expression;
    isa-ok $q, RakuAST::QuotedString, "$src is a QuotedString";
    is $q.processors.join(' '), 'words', "$src: words processor";
    is $q.segments.head.raku, 'RakuAST::StrLiteral.new("a  b")', "$src: raw text";
}
for 'qww/a  b/', 'qqww/a  b/', 'qq:ww/a  b/' -> $src {
    my $q = "my @w = $src;".AST.statements.head.expression.initializer.expression;
    is $q.processors.join(' '), 'quotewords', "$src: quotewords processor";
    is $q.segments.head.raku, 'RakuAST::StrLiteral.new("a  b")', "$src: raw text";
}
for 'my @w = «a  b»;', 'my @w = <<a  b>>;' -> $src {
    my $q = $src.AST.statements.head.expression.initializer.expression;
    is $q.processors.join(' '), 'quotewords val', "$src: quotewords val";
    is $q.segments.head.raku, 'RakuAST::StrLiteral.new("a  b")', "$src: raw text";
}

is EVAL(Q[my @w = qw/a  b/; @w.join('|')].AST), 'a|b', 'qw evaluates';
is EVAL(Q[my @w = qww/a  b/; @w.join('|')].AST), 'a|b', 'qww evaluates';
is EVAL(Q[my @w = «1 b»; @w.map(*.^name).join(',')].AST), 'IntStr,Str',
    '«…» keeps its allomorphs';
is EVAL(Q[my @w = qw/1 b/; @w.map(*.^name).join(',')].AST), 'Str,Str',
    'qw does not make allomorphs';
is EVAL(Q[my @w = qw/a/; @w.elems].AST), 1, 'a one-word qw evaluates';
