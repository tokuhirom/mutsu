use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

plan 7;

my $ast = Q[/ $<x>=a ** {1..3}: b /].AST;
my $body = $ast.statements[0].expression.body;
isa-ok $body, RakuAST::Regex::Sequence, 'the pattern remains a sequence';
isa-ok $body.terms[0], RakuAST::Regex::WithWhitespace,
    'whitespace before the following term is retained';
isa-ok $body.terms[0].regex, RakuAST::Regex::BacktrackModifiedAtom,
    'the trailing ratchet wraps the named capture';
isa-ok $body.terms[0].regex.atom, RakuAST::Regex::NamedCapture,
    'the alias remains the modified atom';
isa-ok $body.terms[0].regex.atom.regex.quantifier,
    RakuAST::Regex::Quantifier::BlockRange,
    'the alias body retains its block-range quantifier';
is $body.terms[0].regex.backtrack.^name,
    'RakuAST::Regex::Backtrack::Ratchet',
    'the modifier is the Ratchet type';

my $match = 'aaab' ~~ EVAL($ast);
is $match<x>.Str, 'aaa', 'EVAL preserves the ratcheted code-count match';
