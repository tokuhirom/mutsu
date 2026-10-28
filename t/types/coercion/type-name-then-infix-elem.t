use Test;

# Distribution: Qwiratry (`ok IO::Path (elem) $navigator.supported-types, ...`).
# A type name is a complete term, so a following `(elem)` / `(cont)` is an
# infix, not a call `Int((elem) ...)`.
plan 8;

my @types = IO::Path, Str;

ok IO::Path (elem) @types, 'a qualified built-in type, set membership';
ok Str (elem) @types, 'a plain built-in type, set membership';
nok Int (elem) @types, 'a type that is not a member';
ok @types (cont) Str, '(cont) with the type on the right';
my $has = IO::Path (elem) @types;
ok $has, 'assignment of the membership result';
is Int(3.9), 3, 'a type call without a space is still a coercion';

sub TEST-ITER(*@a) { @a.elems }
is (TEST-ITER 1, 2, 3), 3, 'an all-caps routine is still a listop head';
sub Titled($x) { $x + 1 }
is (Titled (1)), 2, 'a declared capitalised routine still takes a spaced argument list';
