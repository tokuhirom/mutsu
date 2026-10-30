use experimental :rakuast;
use Test;

# `Origin::Source.prepost-of-pos` (the excerpt a ===SORRY!=== shows around
# <HERE>) is called by static's fail-on-* modes (#15666). Expected values are
# rakudo 2026.09's.

plan 7;

my $ast = Q|sub text() { "wrong" }; my Int $value = text();
say 1;|.AST;
my $source = $ast.statements[1].origin.source;

is-deeply $source.prepost-of-pos(24).List, ('sub text() { "wrong" }; ', 'my Int $value = text();'),
    'a position mid-line splits the line';
is-deeply $source.prepost-of-pos(0).List, ('<BOL>', 'sub text() { "wrong" }; my Int $value = '),
    'the start of a line is <BOL>; the post part is cut to 40 characters';
is-deeply $source.prepost-of-pos(60).List, ('say 1;', '<EOL>'),
    'a position past the end is the end of the last line';
is-deeply $source.prepost-of-pos(47).List, (q|t() { "wrong" }; my Int $value = text();|, '<EOL>'),
    'the end of a line is <EOL>; the pre part is cut to 40 characters';
is-deeply $source.prepost-of-pos(48).List, ('<BOL>', 'say 1;'), 'the start of the second line';
is $source.prepost-of-pos(24).elems, 2, 'it answers a pre and a post part';
is $source.prepost-of-pos(24).^name, 'List', 'as a List';
