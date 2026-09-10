use v6;
use Test;

# A method call interpolated in a string may take arguments that contain the
# string's own delimiter quote and/or a comma, e.g. `"@a.join(", ")"`. mutsu's
# interpolation parser split the argument list on every comma (splitting the
# quoted `", "` literal into two broken fragments) and did not skip quotes when
# matching the closing paren, so the separator argument was dropped and
# `.join(", ")` behaved like `.join()`.

plan 6;

my @a = <a b c>;
is "list: @a.join(", ")!", 'list: a, b, c!',
    'interpolated .join with a quoted "," argument keeps the separator';

is "x: @a.join(" | ")", 'x: a | b | c',
    'interpolated .join with a spaced quoted separator';

my $s = 'a)b)c';
is "y: $s.subst(")", "-")", 'y: a-b)c',
    'interpolated .subst with a close-paren inside a quoted argument';

# Single-quoted argument inside a double-quoted string.
is "z: @a.join(', ')", 'z: a, b, c',
    'interpolated .join with a single-quoted separator';

# A comma-free quoted argument still works (no regression).
is "u: @a.join("-")", 'u: a-b-c',
    'interpolated .join with a simple quoted separator';

# No-arg method chains (with parens) are unaffected.
is "v: @a.elems()", 'v: 3', 'interpolated no-arg method call still works';
