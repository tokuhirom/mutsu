use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

plan 17;

my $words = Q[qw/ foo bar "first second" /].AST.statements[0].expression;
is $words.^name, 'RakuAST::QuotedString', 'a nonplain qw remains a quote node';
is $words.processors.join('|'), 'words', 'qw uses the words processor';
is $words.segments.elems, 1, 'words quote keeps one source-text segment';
is $words.segments[0].value, ' foo bar "first second" ',
    'quotes inside a words quote remain literal text';
is EVAL($words).join('|'), 'foo|bar|"first|second"',
    'a nonplain words quote lowers with word splitting';

my $spaced = Q[q:w/ foo bar "first second" /].AST.statements[0].expression;
is $spaced.processors.join('|'), 'words', 'q:w uses the same processor';
is EVAL($spaced).join('|'), 'foo|bar|"first|second"',
    'a spaced words adverb round-trips';

my $quoted = Q[q:ww/ foo bar "first second" /].AST.statements[0].expression;
is $quoted.^name, 'RakuAST::QuotedString', 'a quoted word list remains a quote node';
is $quoted.processors.join('|'), 'quotewords', 'q:ww uses the quotewords processor';
is $quoted.segments[1].^name, 'RakuAST::QuoteWordsAtom',
    'the quoted word has a dedicated segment';
is EVAL($quoted).join('|'), 'foo|bar|first second',
    'a quoted word remains one element after EVAL';

is EVAL(Q[my @a = qw/ foo bar 'first second' /; @a.elems].AST), 4,
    'a quoted-looking word list round-trips in a declaration';

is EVAL(Q[Qww["a b" \ {1+1}]].AST).join('|'), 'a b|\|{1+1}',
    'Qww keeps unquoted text literal instead of interpolating a block';
is Q[« one #`«comment» two »].AST.statements[0].expression.segments[0].value,
    ' one  two ', 'embedded comments are removed from angle quote source segments';
is EVAL(Q[<< one #`<<comment>> two >>].AST).join('|'), 'one|two',
    'ASCII angle quotes skip embedded comments after round-trip';
is EVAL(Q[q:ww/"#literal" foo/].AST).join('|'), '#literal|foo',
    'a hash sign inside a quoted atom is not a comment';
throws-like { Q[Q:b[\Y]].AST }, X::Backslash::UnrecognizedSequence,
    'an unknown backslash escape keeps its typed parse error';
