use Test;

# A role mixed into a string (`"x" but R`) is still a Str: infix `~` and the
# string comparators read its payload past any `Str`/`Stringy` the role
# declares, and interpolation honours only a declared `Stringy`. Before, a
# role `method Str { self ~ "" }` (Needle::Compile's `Type` role) recursed
# forever.

plan 10;

my role Type { has $.type; method Str (Type:D:) { self ~ "" } }
my $t = "foo" but Type;
is $t.Str, "foo", 'a Str method built on ~ self does not recurse';
is $t ~ "!", "foo!", 'infix ~ reads the payload';

my $x = "x" but role { method Str { "y" } };
is $x ~ "!", "x!", 'infix ~ ignores the role Str';
is "a$x", "ax", 'interpolation ignores the role Str';
ok $x eq "x", 'eq ignores the role Str';
is ~$x, "y", 'prefix ~ calls the role Str';
is $x.Str, "y", '.Str calls the role Str';

my $z = "x" but role { method Stringy { "s" } };
is $z ~ "!", "x!", 'infix ~ ignores the role Stringy';
is "a$z", "as", 'interpolation calls the role Stringy';
is "$z", "s", '...also on its own';
