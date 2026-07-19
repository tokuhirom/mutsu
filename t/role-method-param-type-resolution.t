use v6;
use Test;

# A role method parameter type constraint must resolve the same way a class
# method's does. Previously the role-registration validation over-rejected
# several valid forms with "Invalid typename '...' in parameter declaration."
# (regressions surfaced by real ecosystem dists: IdClass, SQL::Abstract).

plan 6;

# 1. A coercion type as a role method parameter (`Int()`).
{
    role R1 { method f(Int() $x --> Str) { "$x" } }
    class C1 does R1 {}
    is C1.new.f("42"), "42", 'coercion-type param in a role method';
}

# 2. A coercion type with an explicit from-type target (`Identifier(Any)`),
#    where the target is a user class forward-declared before the role.
{
    class Identifier { ... }
    role R2 { method as(Identifier(Any) $x) { "ok" } }
    class Identifier { method Str { "i" } }
    class C2 does R2 {}
    is C2.new.as(Identifier.new), "ok", 'user-type coercion param in a role method';
}

# 3. A role that references its own type in a method parameter.
{
    role Expression { method chain(Expression:D $x) { "chained" } }
    class C3 does Expression {}
    is C3.new.chain(C3.new), "chained", 'self-referential role type param';
}

# 4. A sibling type of an enclosing package, referenced by its short name.
lives-ok {
    EVAL q:to/CODE/;
    unit class Outer4;
    class Sibling {}
    role R4 { method take(Sibling $s) { "took" } }
    class Impl does R4 {}
    die "wrong" unless Impl.new.take(Sibling.new) eq "took";
    CODE
}, 'sibling short-name type in a role method (nested package)';

# 5. A compound-named role (`Renderer::SQL`) referencing an enclosing-package
#    sibling by short name — the failing shape in SQL::Abstract.
lives-ok {
    EVAL q:to/CODE/;
    unit class Outer5;
    role Expression {}
    role Renderer::SQL { method render(Expression:D $e) { "rendered" } }
    class Impl does Renderer::SQL {}
    class E does Expression {}
    die "wrong" unless Impl.new.render(E.new) eq "rendered";
    CODE
}, 'compound-role short-name sibling type (nested package)';

# 6. A genuinely undeclared type is still rejected.
{
    my $ok = False;
    try {
        EVAL 'role RBad { method f(NoSuchTypeXYZ $x) { 1 } }; 1';
        $ok = True;
    }
    nok $ok, 'an undeclared role method param type is still rejected';
}
