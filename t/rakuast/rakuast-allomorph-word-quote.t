use Test;

# A single-word `<…>` quote in RakuAST, measured on rakudo 2026.09: a
# `QuotedString` with the `words` and `val` processors over the bracket text.
# mutsu's parser evaluates it to the allomorph (`<42>` is an IntStr); `.AST`
# renders that allomorph's word, and EVAL reads it back through the same
# quote-word evaluation.

plan 10;

my $q = Q[<42>].AST.statements.head.expression;
isa-ok $q, RakuAST::QuotedString, '`<42>` is a QuotedString';
is $q.processors.join(' '), 'words val', 'with the words and val processors';
is $q.segments.head.raku, 'RakuAST::StrLiteral.new("42")', 'over the bracket text';

is EVAL(Q[<42>.^name].AST), 'IntStr', 'an IntStr survives the round trip';
is EVAL(Q[<1.5>.^name].AST), 'RatStr', 'so does a RatStr';
is EVAL(Q[<1e3>.^name].AST), 'NumStr', 'and a NumStr';
is EVAL(Q[<-7>.^name].AST), 'IntStr', 'and a negative IntStr';
is EVAL(Q[my $x = <42>; ($x + 1) ~ ' ' ~ ($x eq '42')].AST),
    '43 True', 'the value keeps both its number and its string';
is EVAL(Q[(1, <2>, 3).map(*.^name).join(',')].AST),
    'Int,IntStr,Int', 'an allomorph inside a list survives it';
is EVAL(Q[<1/2>.^name].AST), 'Rat', 'a rational literal term stays a Rat';
