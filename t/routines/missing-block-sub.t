use Test;

# A bare `sub` term with neither a signature nor a block is a missing block:
# X::Syntax::Missing, what => 'block'.

plan 4;

throws-like 'for () { sub }', X::Syntax::Missing, what => 'block';
throws-like 'my $x = sub', X::Syntax::Missing, what => 'block';

# Valid anonymous subs still parse and run.
my $f = sub { 42 };
is $f(), 42, 'anonymous sub with block works';
my &g = sub ($x) { $x * 2 };
is g(3), 6, 'anonymous sub with signature works';
