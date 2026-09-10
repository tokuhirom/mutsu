use Test;

# A `%` separator's own captures are quantified captures: `<a> *% <sep>` matching
# a single atom (zero separators) must leave `$<sep>` an EMPTY LIST, exactly as a
# zero-iteration `[ <sep> ]*` does. mutsu collected quantified capture names from
# the token's atom only, never from its separator, so `$<sep>` stayed absent
# (Nil) and `@<sep>` was a bogus 1-element `(Nil,)`. A grammar action doing
# `my @ops = @<op>».ast` over a single-element chain then indexed a phantom op
# (99problems-41-to-50.t P47).

plan 9;

grammar Sep {
    token TOP { <a> *%[ <sep> ] }
    token a   { \d }
    token sep { ',' }
}

is Sep.parse('5')<sep>.raku, '[]', 'zero separators: capture is an empty list, not Nil';
is @(Sep.parse('5')<sep>).elems,     0, 'zero separators: 0 elems';
is @(Sep.parse('5,6')<sep>).elems,   1, 'one separator';
is @(Sep.parse('5,6,7')<sep>).elems, 2, 'two separators';

# `+%` and `%%` share the separator path.
grammar SepPlus {
    token TOP { <a> +%% [ <sep> ] }
    token a   { \d }
    token sep { ',' }
}
is @(SepPlus.parse('5')<sep>).elems,    0, '+%% zero separators: 0 elems';
is @(SepPlus.parse('5,6')<sep>).elems,  1, '+%% one separator';
is @(SepPlus.parse('5,6,')<sep>).elems, 2, '+%% counts the trailing separator too';

# A plain quantified group already behaved; keep it pinned so the shared
# `collect_quantified_names_for_token` change does not regress it.
grammar Grp {
    token TOP { <a> [ <sep> <a> ]* }
    token a   { \d }
    token sep { ',' }
}
is Grp.parse('5')<sep>.raku, '[]', 'quantified group: zero iterations is an empty list';
is @(Grp.parse('5,6')<sep>).elems, 1, 'quantified group: one iteration';

# vim: expandtab shiftwidth=4
