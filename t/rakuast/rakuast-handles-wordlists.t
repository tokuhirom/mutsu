use Test;
use experimental :rakuast;

plan 6;

my $source = Q[class ReadWordList { has %.data handles(<keys  Bool>); }];
my $attr = $source.AST.statements[0].expression.body.body.statement-list.statements[0].expression;
my $term = $attr.traits[0].term;
isa-ok $term, RakuAST::Circumfix::Parentheses, 'handles keeps the written parentheses';
my $words = $term.semilist.statements[0].expression;
isa-ok $words, RakuAST::QuotedString, 'the word list stays a quoted string term';
is $words.processors.join(' '), 'words val', 'the word-list processors are preserved';
is $words.segments[0].value, 'keys  Bool', 'the original spacing inside the quote is preserved';

is EVAL(Q[class EvalWordList { has %.data handles(<keys Bool>); }; EvalWordList.new(data => %(answer => 42)).keys.join(',')].AST),
    'answer', 'parenthesized quote words delegate after the AST round trip';
is EVAL(Q[class EvalMixedWords { has %.data handles('elems', <keys Bool>); }; EvalMixedWords.new(data => %(a => 1, b => 2)).elems].AST),
    2, 'mixed quoted names and word lists delegate after the AST round trip';
