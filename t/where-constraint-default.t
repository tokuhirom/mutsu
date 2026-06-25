use Test;

plan 9;

# A `where` constraint must not swallow the following `=` default value.
sub a($x where * > 0 = 9) { $x }
is a(),  9, 'where + default: default applies when arg omitted';
is a(5), 5, 'where + default: passed arg used';

# Type constraint as the where target, followed by a default.
sub b($x where Int = 7) { $x }
is b(),  7, 'where Int + default applies';
is b(3), 3, 'where Int + passed arg used';

# Named parameter with type, where, default, then a `-->` return constraint.
sub c(Str:D :$pad where *.chars <= 1 = '=' --> Str) { $pad }
is c(), '=', 'named typed where + default + return constraint';

# Multi-line signature: where + default, comma, newline, then `-->`.
sub d(
    $blob,
    :$pad where *.chars <= 1 = '=',
--> Int) {
    $pad.chars
}
is d(1), 1, 'multi-line where + default + comma + return constraint';

# The where constraint still rejects invalid values.
sub e($x where * > 0 = 9) { $x }
dies-ok { e(-1) }, 'where still rejects out-of-range value';
lives-ok { e(2) }, 'where accepts valid value';

# Block form of where with a following default still works.
sub f($x where { $_ %% 2 } = 4) { $x }
is f(), 4, 'block where + default';
