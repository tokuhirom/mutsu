use v6;
use Test;
use experimental :rakuast;

# RakuAST::Type::Enum keeps the enum's unevaluated variant term. The parser
# must therefore preserve which quote form produced the normalized variants.
plan 15;

my $words = Q[enum WordColor <Red Green>].AST.statements[0].expression;
is $words.^name, 'RakuAST::Type::Enum', 'word-list enum renders as Type::Enum';
is $words.name.^name, 'RakuAST::Name', 'enum name is a Name node';
is $words.term.^name, 'RakuAST::QuotedString', 'word-list enum keeps a QuotedString term';
is $words.term.processors.raku, '("words", "val")', 'word-list processors are preserved';
is $words.term.segments[0].value, 'Red Green', 'word-list source stays unevaluated';

my $quote-words = Q[enum QuoteColor «Red Green»].AST.statements[0].expression;
is $quote-words.term.processors.raku, '("quotewords", "val")',
    'guillemet enum keeps quotewords processors';

my $pairs = Q[enum PairColor (Red => 5, Green => 6)].AST.statements[0].expression;
is $pairs.term.^name, 'RakuAST::Circumfix::Parentheses',
    'pair-list enum keeps its parenthesized term';
ok $pairs.gist.contains('RakuAST::FatArrow.new('),
    'pair-list enum renders FatArrow operands';
ok !Q["hi"].AST.gist.contains('processors'),
    'ordinary quoted strings still omit the default processors field';

my $word-values = Q[enum EvalWordColor <Red Green>].AST.EVAL;
is $word-values.^name, 'Map', 'evaluating a word-list enum returns its Map';
is $word-values<Red>, 0, 'word-list enum registers the first value';
is $word-values<Green>, 1, 'word-list enum registers the second value';

my $pair-values = Q[enum EvalPairColor (Red => 5, Green => 6)].AST.EVAL;
is $pair-values<Red>, 5, 'evaluating a pair-list enum preserves its first value';
is $pair-values<Green>, 6, 'evaluating a pair-list enum preserves its second value';

my $term = RakuAST::QuotedString.new(
    processors => <words val>,
    segments => (RakuAST::StrLiteral.new('Red Green'),),
);
my $constructed = RakuAST::Type::Enum.new(
    name => RakuAST::Name.from-identifier('WordColor'),
    term => $term,
);
is $constructed.gist, $words.gist, 'Type::Enum and QuotedString constructors round-trip';
