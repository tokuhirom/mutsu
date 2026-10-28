use Test;

# Distribution: Qwiratry (`ok IO::Path (elem) $navigator.supported-types, ...`).
# A type name is a complete term, so a following `(elem)` / `(cont)` is an
# infix, not a call `Int((elem) ...)`.
plan 6;

my @types = IO::Path, Str;

ok IO::Path (elem) @types, 'a qualified built-in type, set membership';
ok Str (elem) @types, 'a plain built-in type, set membership';
nok Int (elem) @types, 'a type that is not a member';
ok @types (cont) Str, '(cont) with the type on the right';
my $has = IO::Path (elem) @types;
ok $has, 'assignment of the membership result';
is Int(3.9), 3, 'a type call without a space is still a coercion';
