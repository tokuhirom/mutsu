use Test;

# Nested sub-captures are shared behind an Arc during matching (a parent
# RegexCaptures clone is a refcount bump, not a deep copy of the whole
# sub-match tree). This pins that the sharing is transparent: every nested
# capture must resolve to its own correct text and structure, and repeated /
# quantified sub-captures must not alias each other.

plan 9;

grammar Nested {
    token TOP     { <outer>+ % ';' }
    token outer   { '(' <inner>+ % ',' ')' }
    token inner   { <key> '=' <val> }
    token key     { <[a..z]>+ }
    token val     { <[0..9]>+ }
}

my $m = Nested.parse('(a=1,bb=22);(ccc=333)');
ok $m, 'deeply nested grammar parses';

# Two top-level <outer> groups.
is $m<outer>.elems, 2, 'two outer groups captured';

# First outer has two inner pairs, second has one.
is $m<outer>[0]<inner>.elems, 2, 'first outer has two inner pairs';
is $m<outer>[1]<inner>.elems, 1, 'second outer has one inner pair';

# Each nested key/val resolves to its own text (no aliasing between shared subcaps).
is $m<outer>[0]<inner>[0]<key>.Str, 'a',   'inner[0].key correct';
is $m<outer>[0]<inner>[0]<val>.Str, '1',   'inner[0].val correct';
is $m<outer>[0]<inner>[1]<key>.Str, 'bb',  'inner[1].key correct';
is $m<outer>[0]<inner>[1]<val>.Str, '22',  'inner[1].val correct';
is $m<outer>[1]<inner>[0]<key>.Str, 'ccc', 'second group inner key correct';
