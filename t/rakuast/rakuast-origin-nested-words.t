use experimental :rakuast;
use Test;

# The literal runs around a nested angle quote include its brackets; the
# nested quote itself and its text cover only the characters inside them.
plan 10;

sub positions($nodes) {
    $nodes.map({
        .^name.split('::').tail ~ '@' ~ .origin.from ~ '-' ~ .origin.to
    }).join(' ')
}

my $quote = Q[<a <b c> d>].AST.statements[0].expression;
is positions($quote.segments),
    'StrLiteral@1-4 QuotedString@4-7 StrLiteral@7-10',
    'nested word quote segments have source ranges';
is positions($quote.segments[1].segments), 'StrLiteral@4-7',
    'the inner literal has the inner quote range';

my $deep = Q[<a <b <c>> d>].AST.statements[0].expression;
is positions($deep.segments),
    'StrLiteral@1-4 QuotedString@4-9 StrLiteral@9-12',
    'outer segments of a deeply nested quote';
is positions($deep.segments[1].segments),
    'StrLiteral@4-7 QuotedString@7-8 StrLiteral@8-9',
    'inner segments of a deeply nested quote';
is positions($deep.segments[1].segments[1].segments), 'StrLiteral@7-8',
    'deepest literal keeps its range';

my $postfix = Q[my $m; $m<value:sym<number>>].AST.statements[1].expression.postfix;
is $postfix.origin.from ~ '-' ~ $postfix.origin.to, '9-28',
    'the subscript spans its outer brackets';
my $subscript = $postfix.index;
is $subscript.origin.from ~ '-' ~ $subscript.origin.to, '10-27',
    'the quote spans the text inside the subscript brackets';
is positions($subscript.segments),
    'StrLiteral@10-20 QuotedString@20-26 StrLiteral@26-27',
    'a word quote in a subscript has segment ranges';
is positions($subscript.segments[1].segments), 'StrLiteral@20-26',
    'the nested subscript quote has a literal range';

is positions(Q[<é <b> z>].AST.statements[0].expression.segments),
    'StrLiteral@1-4 QuotedString@4-5 StrLiteral@5-8',
    'ranges count characters rather than UTF-8 bytes';
