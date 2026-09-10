use Test;

plan 11;

# X::TypeCheck::Binding::Parameter - exact type match
throws-like { my &f = sub (Int $x) {}; f("hello") },
    X::TypeCheck::Binding::Parameter,
    "binding parameter type check - exact match";

# X::TypeCheck::Binding - parent type matches child exception
throws-like { my &f = sub (Int $x) {}; f("hello") },
    X::TypeCheck::Binding,
    "binding type check - parent type matches";

# X::TypeCheck::Binding::Parameter with attribute matchers.
#
# raku exposes `.expected` as the expected TYPE OBJECT and `.got` as the
# offending VALUE -- not their names -- so the matchers are `Int` and `"hello"`,
# not `/Int/` and `/Str/`. (The regex spellings were mutsu-shaped: they failed
# under `raku t/exception-types.t` too, and only passed here while mutsu stored
# the got TYPE NAME instead of the value.)
throws-like { my &f = sub (Int $x) {}; f("hello") },
    X::TypeCheck::Binding::Parameter,
    expected => Int,
    got => "hello",
    "binding parameter type check with attribute matchers";

# X::AdHoc - a `:=`-bound literal has no container at all, so rakudo reports
# the generic "Cannot assign to an immutable value", NOT X::Assignment::RO
# (which is reserved for modifying an immutable *value*, e.g. a `constant`).
throws-like { my $x := 42; $x = 43 },
    X::AdHoc,
    "cannot assign to readonly bound variable",
    message => /'Cannot assign to an immutable value'/;

# X::Assignment::RO - readonly Set
throws-like { my $s = set <a b c>; $s<d> = True },
    X::Assignment::RO,
    "cannot modify immutable Set";

# X::Assignment::RO - readonly Bag
throws-like { my $b = bag <a b c>; $b<d> = 1 },
    X::Assignment::RO,
    "cannot modify immutable Bag";

# X::Assignment::RO - readonly Mix
throws-like { my $m = mix <a b c>; $m<d> = 1.5 },
    X::Assignment::RO,
    "cannot modify immutable Mix";

# X::TypeCheck - grandparent matches
throws-like { my &f = sub (Int $x) {}; f("hello") },
    X::TypeCheck,
    "X::TypeCheck grandparent matches X::TypeCheck::Binding::Parameter";

# X::AdHoc with message attribute (a `:=`-bound literal, see above)
throws-like { my $x := 42; $x = 43 },
    X::AdHoc,
    message => /Cannot/,
    "immutable-bind message attribute matches";

# X::Assignment::RO IS the right class for modifying an immutable *value*:
# a sigilless `constant` term is the value itself, not a variable.
throws-like { my constant PI = 3.14; PI = 5 },
    X::Assignment::RO,
    message => /'Cannot modify an immutable Rat (3.14)'/,
    "constant term assignment throws X::Assignment::RO";

# Exception - top-level parent matches any exception
throws-like { my &f = sub (Int $x) {}; f("hello") },
    Exception,
    "Exception matches any typed exception";
