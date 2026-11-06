use Test;

# X::Assignment::RO names a role-mixin value by its role, not `Any`
# (Language/subscripts.rakudoc).
plan 4;

role Foo {}

sub msg(&c) { try c(); $!.message }

is msg({ my @a is Foo = 1, 2, 3 }), 'Cannot modify an immutable Foo (Foo.new)', 'an array is Foo';
is msg({ my %h is Foo = a => 42 }), 'Cannot modify an immutable Foo (Foo.new)', 'a hash is Foo';
is msg({ 5 = 6 }), 'Cannot modify an immutable Int (5)', 'a plain Int is unchanged';

my \v = 42 but role { };
try { v = 1 };
like $!.message, /'Cannot modify an immutable Int+{' .* '} (42)'/, 'a but-mixin keeps its mixin name';
