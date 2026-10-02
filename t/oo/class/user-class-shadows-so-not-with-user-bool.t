use Test;

# A class that defines both `Bool` and its own `.not` / `.so` must have its
# own methods called, not the native truthiness fast path.
# Source: Logic::Ternary (zef distribution), t/01-basic.rakutest.

plan 6;

class Tern {
    has $.v;
    method Bool { $!v > 0 }
    method not  { Tern.new(v => -$!v) }
    method so   { self }
}

my $t = Tern.new(v => 1);
is-deeply $t.not.v, -1, '.not on a variable calls the user method';
is-deeply Tern.new(v => 2).not.v, -2, '.not on a call result calls the user method';
is-deeply $t.so.v, 1, '.so calls the user method';
isa-ok $t.not, Tern, '.not returns the user type, not Bool';

class OnlyBool { method Bool { False } }
is-deeply OnlyBool.new.not, True, '.not still goes through a user Bool';
is-deeply OnlyBool.new.so, False, '.so still goes through a user Bool';
