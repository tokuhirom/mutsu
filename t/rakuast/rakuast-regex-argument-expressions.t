use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

plan 8;

my $ordinal = Q[m:st(1|8)/fo+/].AST;
my $ordinal_regex = $ordinal.statements[0].expression;
my $ordinal_adverb = $ordinal_regex.adverbs[0];
isa-ok $ordinal_adverb, RakuAST::ColonPair::Value,
    'a junction-valued ordinal remains a colonpair value';
is $ordinal_adverb.key, 'st', 'the ordinal adverb key is retained';
isa-ok $ordinal_adverb.value, RakuAST::Circumfix::Parentheses,
    'the ordinal junction remains parenthesized';
ok $ordinal_adverb.value.gist.contains('RakuAST::ApplyListInfix'),
    'the ordinal remains an ApplyListInfix';
ok $ordinal_adverb.value.gist.contains('RakuAST::Infix.new("|")'),
    'the junction operator remains the vertical bar';

my $name = Q[/ <:name(/:s LATIN SMALL LETTER/)> /].AST;
my $name_gist = $name.gist;
ok $name_gist.contains('RakuAST::Regex::CharClassElement::Property'),
    'the Unicode name property remains a property element';
ok $name_gist.contains('predicate => RakuAST::Circumfix::Parentheses'),
    'the property predicate keeps its parenthesized nested regex';
ok 'a' ~~ EVAL($name),
    'EVAL lowers the nested regex property predicate';
