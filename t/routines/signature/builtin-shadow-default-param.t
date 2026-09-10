use Test;

plan 9;

# A user-declared routine shadows a same-named builtin: in raku a lexical `sub
# abs` (declared here or imported) wins over CORE's. mutsu's named-call path
# runs the user def directly, but only when its strict builtin-shadow gate
# accepts it; that gate rejects a signature with a DEFAULT parameter (a
# name-cache hazard, PR #3546) and control then fell through to the NATIVE
# builtin instead of to the user def. So `sub rotate (Str $s, Int $n = 1)` lost
# every call, while the same sub without the default won — see
# t/imported-sub-shadows-builtin.t for the no-default shape.

sub rotate (Str $s, Int $n = 1 --> Str) { "R:$s/$n" }
sub abs    (Str $s, Int $n = 1 --> Str) { "A:$s/$n" }
sub elems  (Str $s, Int $n = 1 --> Str) { "E:$s/$n" }

is rotate('x', 3), 'R:x/3',
    'a shadowing sub with a default wins when the arg count matches a builtin arity';
is rotate('x'),    'R:x/1', 'and when the default is taken';
is abs('x', 3),    'A:x/3', 'a shadow already won where no builtin of that arity exists';
is abs('x'),       'A:x/1', 'and now wins where one does';
is elems('x', 3),  'E:x/3', 'likewise for another builtin name';
is elems('x'),     'E:x/1', 'and its 1-arg form';

# A shadow with no default was already correct; pin it so the change cannot
# regress the shape the strict gate does accept.
sub sign (Str $s --> Str) { "S:$s" }
is sign('x'), 'S:x', 'a shadow with no default still wins';

# The builtins must still be reachable where nothing shadows them.
is (1, 2, 3).rotate(1).join(','), '2,3,1', 'the builtin rotate METHOD is untouched';
is reverse(1, 2, 3).join(','),    '3,2,1', 'an unshadowed builtin sub still dispatches natively';
