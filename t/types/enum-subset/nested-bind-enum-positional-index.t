use Test;

# A numeric enum value as a positional subscript of a nested store names the
# element its value does: `@a[B][9] := v` binds `@a[1][9]`. The nested store
# keyed each level by its text, so `"B"` addressed no element and the bind
# landed in `@a[9]`. Found in Terminal::ANSIParser, whose
# `@actions[$_][0x7F] := &ignore-byte` (with `$_` a DecodeState) grew
# `@actions` to 128 entries and lost the DEL handling (#17429).

plan 6;

enum E <A B C D>;

{
    my @a = [], [], [], [];
    @a[B][9] := 1;
    is @a.elems, 4, 'binding through an enum index does not grow the outer array';
    is @a[1][9], 1, '... and binds the element its value names';
}

{
    my @a = [], [], [], [];
    for B, C { @a[$_][3] := 'x' }
    is-deeply (@a[1][3], @a[2][3]), ('x', 'x'), 'with the enum as the loop topic';
    is @a.elems, 4, '... no extra elements';
}

{
    my @a = [], [];
    @a[True][2] := 5;
    is @a[1][2], 5, 'a Bool index names its Int too';
}

{
    my %h;
    %h<B><x> := 3;
    is-deeply %h.keys.List, ('B',), 'an associative level keeps the text key';
}
