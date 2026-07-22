use Test;

# A parenthesized (expression-context) indexed assignment/bind must carry the
# SAME `is_positional` flag as its bare-statement form. The expression-context
# parser (`try_parse_assign_expr`, used for `(...)` groups) used to hardcode
# `is_positional: true` for every subscript shape, so a `(%h<k> := v)` /
# `(%h<k> = v)` was compiled as a POSITIONAL element assign. For a user
# Associative object that routed to the non-existent BIND-POS/ASSIGN-POS, which
# fell through to the generic path and REPLACED the whole tied container.
# Regression pin for the Hash::Agnostic dist (`%h<i> := 137` in `t/01-basic`).

plan 12;

# --- plain hash: associative subscript stays associative through parens -------
my %h = (a => 1, b => 2, c => 3);
(%h<d> := 4);
is %h.elems, 4, 'paren <> bind adds one key (not a whole-hash replace)';
is %h<d>, 4, 'paren <> bind stored the value';
is %h.keys.sort.join(","), 'a,b,c,d', 'paren <> bind kept the other keys';

(%h<e> = 5);
is %h.elems, 5, 'paren <> assign adds one key';
is %h<e>, 5, 'paren <> assign stored the value';

%h{'f'} := 6;   # brace subscript, bare
is %h.elems, 6, 'brace bind adds one key';
is %h<f>, 6, 'brace bind stored the value';

(%h{'g'} = 7);  # brace subscript, parenthesized
is %h.elems, 7, 'paren brace assign adds one key';

# --- plain array: positional subscript stays positional through parens --------
my @a = 10, 20, 30;
(@a[5] := 99);
is @a[5], 99, 'paren [] bind stored the value at the index';
is @a.elems, 6, 'paren [] bind extended the array';

(@a[1] = 21);
is @a[1], 21, 'paren [] assign updated the element';
is @a.elems, 6, 'paren [] assign did not change elems';
