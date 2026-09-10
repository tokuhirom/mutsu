use Test;

plan 5;

# An `is required` `:D` attribute that IS provided but with an undefined value
# fails the assignment typecheck (X::TypeCheck::Assignment), while a missing
# one still raises X::Attribute::Required (JSON::Unmarshal 040 test 20).

class IntDClass {
    has Int:D $.attr is required;
}

throws-like { IntDClass.new(attr => Int) }, X::TypeCheck::Assignment,
    'undefined value into a provided :D required attr throws typecheck';

throws-like { IntDClass.new }, X::Attribute::Required,
    'missing :D required attr still throws X::Attribute::Required';

is IntDClass.new(attr => 42).attr, 42, 'defined value accepted';

# A BUILD submethod defers the check: the pre-BUILD assembly must not fire it.
# (mutsu raises X::Attribute::Required here — pinned by
# t/class-build-invocant-required.t — while rakudo 2026.06 raises
# X::TypeCheck::Assignment; either way it must die, and a provided value
# must construct cleanly.)
my class Box {
    has Any:D $.value is required;
    submethod BUILD(::?CLASS:D: :$!value) { $!value }
}
dies-ok { Box.new }, 'BUILD class: missing required attr dies';
is Box.new(value => 42).value, 42, 'BUILD class: provided value works';
