use Test;

plan 4;

# The X::Assignment::RO text names the mixin type of an immutable sigilless
# value, also when the assignment happens inside a closure.
my \v = 42 but role R { };

sub msg(&c) { try c(); $!.message }

is msg({ v = 1 }), 'Cannot modify an immutable Int+{R} (42)', 'assignment inside a closure';
is msg(sub { v = 1 }), 'Cannot modify an immutable Int+{R} (42)', 'assignment inside an anonymous sub';
try { v = 1 };
is $!.message, 'Cannot modify an immutable Int+{R} (42)', 'assignment inline';
my &d = { try { v = 1 }; $!.message };
is d(), 'Cannot modify an immutable Int+{R} (42)', 'assignment inside a stored block';

# vim: expandtab shiftwidth=4
