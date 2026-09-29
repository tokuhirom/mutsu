use v6;
use Test;

# `OP=` on a non-container value names the value in X::Assignment::RO, like
# the plain `=` form does (#9811, #9956).
plan 9;

my @t = 0, 10;
try { @t[1]:v += 31; }
isa-ok $!, X::Assignment::RO, '@t[1]:v += 31 dies with X::Assignment::RO';
is $!.message, 'Cannot modify an immutable Int (10)', 'message names the element value';

my %h = a => 1;
try { %h<a>:v ~= "x"; }
isa-ok $!, X::Assignment::RO, '%h<a>:v ~= dies with X::Assignment::RO';
is $!.message, 'Cannot modify an immutable Int (1)', 'message names the hash value';

sub f { 10 }
try { f() += 1; }
isa-ok $!, X::Assignment::RO, 'f() += 1 on a non-rw sub dies with X::Assignment::RO';
is $!.message, 'Cannot modify an immutable Int (10)', 'message names the returned value';

try { f() = 3; }
is $!.message, 'Cannot modify an immutable Int (10)', 'plain f() = 3 names the returned value';

my @u = 5, 6;
lives-ok { @u[1]:v //= 9 }, 'short-circuit //= on a defined :v value does not assign';
is @u, [5, 6], 'array untouched';
