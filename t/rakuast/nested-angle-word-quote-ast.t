use Test;
use experimental :rakuast;

plan 11;

# A balanced `<...>` inside a `<...>` word quote is a nested word quote, a
# segment of its own between literal runs that keep the brackets, as rakudo
# parses it (#18457). The words it evaluates to do not change.

sub segments-of($node) { $node.segments.map({ $_ ~~ RakuAST::StrLiteral ?? .value !! .^name }).List }

my $words = Q[<a <b c> d>].AST.statements[0].expression;
is-deeply segments-of($words), ('a <', 'RakuAST::QuotedString', '> d'), '<a <b c> d>';
is-deeply $words.segments[1].segments.map(*.value).List, ('b c',), 'the nested quote';
is-deeply Q[<a <b c> d>].AST.EVAL, <a <b c> d>, 'EVAL gives the same words';

my $index = Q[my $m; $m<value:sym<number>>].AST.statements[1].expression.postfix.index;
is-deeply segments-of($index), ('value:sym<', 'RakuAST::QuotedString', '>'), 'a subscript';
my %h = 'value:sym<number>' => 1;
is Q[my %h = 'value:sym<number>' => 1; %h<value:sym<number>>].AST.EVAL, 1, 'EVAL of the subscript';

my $deep = Q[<a <b <c>> d>].AST.statements[0].expression;
is $deep.segments[1].segments[1].^name, 'RakuAST::QuotedString', 'nesting nests';
is-deeply Q[<a <b <c>> d>].AST.EVAL, <a <b <c>> d>, 'EVAL of the deeper nesting';

is Q[<a b>].AST.statements[0].expression.segments.elems, 1, 'no inner brackets: one literal';
is Q[«a <b> c»].AST.statements[0].expression.segments.elems, 1, '«...» does not nest';
is Q[<a <b c> d>].AST.DEPARSE.trim, Q[<a \<b c\> d>], 'DEPARSE';
is-deeply Q[<x<y>>].AST.EVAL, 'x<y>', 'a single word with brackets';
