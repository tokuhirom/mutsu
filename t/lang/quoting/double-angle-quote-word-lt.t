use Test;

plan 15;

# A `<` inside `<< >>` is word text, not a nested opener: only the closing
# `>>` is special. `<< < ≤ <= >>` used to be a parse error ("Confused").
# Issue #9325.

is << < >>, '<', 'a lone < is a word';
is-deeply << a <= >>, ('a', '<='), 'a word starting with < is a word';
is-deeply (0, 1, << < ≤ <= >>), (0, 1, ('<', '≤', '<=')), 'the autocomplete list from Jupyter::Kernel';
is << a<b >>, 'a<b', 'a < inside a word is kept';
is-deeply << <a b> c >>, ('<a', 'b>', 'c'), 'a <...> pair inside << >> is not quote protection';
is-deeply << a> b >>, ('a>', 'b'), 'a > not followed by > is word text';
is-deeply «a < b», ('a', '<', 'b'), 'the same in « »';

my %h = a => 1, b => 2;
is <<%h<a> z>>.join('|'), '1|z', 'a subscripted interpolation still works';
my @w = <<x %h<b>>>;
is @w.join('|'), 'x|2', 'a subscript right before the closing >> still works';
is-deeply (:k<< a<b c >>), (k => ('a<b', 'c')), 'the colonpair form';

# Only a full copy of the list's own opener nests, and it splits off from the
# word it is glued to.
is-deeply << a <<b>> c >>, ('a', '<<', 'b', '>>', 'c'), 'a nested << >> pair nests';
is-deeply «a «b» c», ('a', '«', 'b', '»', 'c'), 'a nested « » pair nests';
is-deeply << a<<b>>c >>, ('a<<', 'b', '>>c'), 'a nested opener ends its word, a nested closer starts one';
is-deeply << a «b» >>, ('a', '«b»'), '« » is word text inside << >>';
is-deeply « a <<b>> », ('a', '<<b>>'), '<< >> is word text inside « »';
