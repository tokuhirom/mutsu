use Test;

# Passing a non-writable argument (a literal, an itemized array) to an `is
# rw` parameter must raise the typed X::Parameter::RW, not the generic
# X::AdHoc. mutsu's binding-error "enhancement" step (which wraps a failed
# call's error in "Calling f(Int) will never work with declared signature
# (...)") used to swallow this specific class, because the RW check spells
# its class only via the "X::Type: text" message convention (no `.exception`
# object attached) and the wrap prepended text in front of that convention
# prefix, breaking the later class-recovery parse.

plan 4;

sub takes-rw ($x is rw) { $x }

try { takes-rw(1) };
is $!.^name, 'X::Parameter::RW',
    'a literal argument to an is rw parameter raises X::Parameter::RW';

try { takes-rw($[1, 2]) };
is $!.^name, 'X::Parameter::RW',
    'an itemized array argument to an is rw parameter also raises X::Parameter::RW';

my $writable = 5;
is takes-rw($writable), 5, 'a genuine writable variable still binds fine';

sub takes-raw (\x) { x }
try { takes-raw(1) };
isnt $!.^name, 'X::Parameter::RW',
    'a plain sigilless \\x parameter (not is rw) is unaffected by the fix';
