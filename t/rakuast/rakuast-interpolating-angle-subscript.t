use Test;

# `%h<<$key>>` / `%h«a "b c" $k»` in RakuAST, measured on rakudo 2026.09: the
# subscript is a `Postcircumfix::LiteralHashIndex` whose index is a
# `QuotedString(processors => <quotewords val>, ...)` over the text as written;
# an unquoted run is a `StrLiteral`, a quoted word a `QuoteWordsAtom`, a
# variable a `Var::Lexical`. EVAL of the tree looks up the same keys.

plan 13;

sub index-of($src) { $src.AST.statements[*-1].expression.postfix.index }

my $one = index-of(Q[my %h; my $key; %h<<$key>>]);
isa-ok $one, RakuAST::QuotedString, 'the index is a QuotedString';
is $one.processors.join(','), 'quotewords,val', 'with the quotewords and val processors';
isa-ok $one.segments[0], RakuAST::Var::Lexical, 'a variable is a Var::Lexical segment';

my $plain = index-of(Q[my %h; %h<<a b>>]);
is $plain.processors.join(','), 'quotewords,val', 'plain words keep the raw text';
isa-ok $plain.segments[0], RakuAST::StrLiteral, 'as one StrLiteral';

my $mixed = index-of(Q[my %h; my $k; %h«a "b c" $k»]);
is $mixed.segments.map(*.^name).join(','),
    'RakuAST::StrLiteral,RakuAST::QuoteWordsAtom,RakuAST::StrLiteral,RakuAST::Var::Lexical',
    'a quoted word is a QuoteWordsAtom';

my $assign = Q[my %h; my $k; %h<<$k>> = 1].AST.statements[2].expression;
isa-ok $assign.postfix, RakuAST::Postcircumfix::LiteralHashIndex, 'an assigned subscript keeps the class';

sub run($src) { EVAL($src.AST) }
is run(Q[my %h = a => 1, b => 2; my $key = 'a'; %h<<$key>>]), 1, 'an interpolated key';
is run(Q[my %h = a => 1, b => 2; %h<<a b>>.join(',')]), '1,2', 'plain words slice';
is run(Q[my %h = a => 1, 'b c' => 2; my $k = 'a'; (%h«"b c" $k»).join(',')]), '2,1',
    'a quoted word and a variable';
is run(Q[my %h; my $k = 'x'; %h<<$k>> = 5; %h<x>]), 5, 'assignment through the subscript';
is run(Q[my %h = a => 1; my $key = 'a'; %h<<$key>>:exists]), True, 'with an :exists adverb';
is run(Q[my %h = a => 1; my $key = 'a'; %h«$key»:delete; %h.elems]), 0, 'with a :delete adverb';
