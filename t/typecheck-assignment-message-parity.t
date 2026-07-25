use Test;

plan 10;

# Rakudo's X::TypeCheck::Assignment message is
#   Type check failed in assignment to $x; expected Int but got Str ("s")
# — `but got`, not `, got`, and with the offending value's short repr appended.
# Several mutsu paths formatted their own short template instead.

try { my Int $x = "s" };
is $!.message, 'Type check failed in assignment to $x; expected Int but got Str ("s")',
    'a typed `my` declaration uses the full Rakudo wording';

class C { }
try { my C $y = 3 };
is $!.message, 'Type check failed in assignment to $y; expected C but got Int (3)',
    'and names a user class the same way';

# The `.new` attribute path raised an untyped AdHoc with the short wording.
class Foo { has Int $.n }
try { Foo.new(n => "s") };
is $!.message, 'Type check failed in assignment to $!n; expected Int but got Str ("s")',
    'the .new attribute path uses the full wording';
isa-ok $!, X::TypeCheck::Assignment, 'and raises a typed X::TypeCheck::Assignment';
is $!.expected, Int, '.expected is the expected type object';
is $!.got, "s", '.got is the offending value';

# Rakudo always names the ATTRIBUTE, whichever syntax wrote it: assigning through
# the `is rw` accessor reports `$!n`, not the source-level `$.n`.
class Bar { has Int $.n is rw; method set($v) { $.n = $v } }
try { Bar.new.set("s") };
is $!.message, 'Type check failed in assignment to $!n; expected Int but got Str ("s")',
    'an `is rw` accessor assignment inside a method names $!n, not $.n';

class Baz { has Int $.n is rw }
try { Baz.new.n = "s" };
is $!.message, 'Type check failed in assignment to $!n; expected Int but got Str ("s")',
    'and so does an accessor assignment from outside';

# The element and return-value messages already matched; pin them so the shared
# formatter cannot regress them while the assignment wording changes.
my Int @a;
try { @a[0] = "s" };
is $!.message, 'Type check failed for an element of @a; expected Int but got Str ("s")',
    'the array-element message is unchanged';

sub f(--> Int) { "s" }
try { f() };
is $!.message, 'Type check failed for return value; expected Int but got Str ("s")',
    'the return-value message is unchanged';
