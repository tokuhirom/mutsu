use Test;

plan 10;

# X::TypeCheck::Binding::Parameter - exact type match
throws-like { my &f = sub (Int $x) {}; f("hello") },
    X::TypeCheck::Binding::Parameter,
    "binding parameter type check - exact match";

# X::TypeCheck::Binding - parent type matches child exception
throws-like { my &f = sub (Int $x) {}; f("hello") },
    X::TypeCheck::Binding,
    "binding type check - parent type matches";

# X::TypeCheck::Binding::Parameter with attribute matchers
throws-like { my &f = sub (Int $x) {}; f("hello") },
    X::TypeCheck::Binding::Parameter,
    expected => /Int/,
    got => /Str/,
    "binding parameter type check with attribute matchers";

# X::Assignment::RO - readonly variable
throws-like { my $x := 42; $x = 43 },
    X::Assignment::RO,
    "cannot assign to readonly bound variable";

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

# X::Assignment::RO with message attribute
throws-like { my $x := 42; $x = 43 },
    X::Assignment::RO,
    message => /Cannot/,
    "X::Assignment::RO message attribute matches";

# Exception - top-level parent matches any exception
throws-like { my &f = sub (Int $x) {}; f("hello") },
    Exception,
    "Exception matches any typed exception";
