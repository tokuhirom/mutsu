use Test;

plan 6;

# A pointy block may take a string-literal parameter, matching the argument
# against that literal — the same way the numeric form `-> 42 { }` works. Cro
# routes literal path segments this way: `get -> 'home.html' { ... }`. mutsu
# handled numeric literals but not string literals, so `-> 'x' { }` failed to
# parse ("Confused ... right-hand expression").

# Parses (a genuine parse error would throw at compile time).
my $single = -> 'home.html' { 'ok-single' };
ok $single ~~ Callable, "-> 'literal' { } parses (single-quoted)";

my $double = -> "home.html" { 'ok-double' };
ok $double ~~ Callable, '-> "literal" { } parses (double-quoted)';

# The literal parameter matches an equal argument.
is $single('home.html'), 'ok-single', 'string-literal param matches the equal argument';

# A literal followed by a normal positional (`-> 'x', $y { }`).
my $mixed = -> 'get', $path { "got-$path" };
is $mixed('get', '/x'), 'got-/x', 'literal + positional pointy params';

# Multi-candidate dispatch by literal (the numeric form still works too).
my $num = -> 42 { 'forty-two' };
is $num(42), 'forty-two', 'numeric literal pointy param still works';

# A pointy block with no parameter is unaffected.
my $none = -> { 'no-param' };
is $none(), 'no-param', '-> { } (no param) unaffected';
