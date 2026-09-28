use Test;

# A role method's parameter type constraint that names a well-known compound
# (`::`-qualified) CORE type -- one mutsu doesn't back with a full registry
# entry, but whose existence raku-doc's type graph pins -- was rejected as
# "Invalid typename", even though the identical signature on a sub or a class
# method resolved it fine. `is_resolvable_type` (the only check the role-method
# validator relies on) consulted `is_known_type_constraint` for an unqualified
# builtin name but never its compound sibling `is_known_compound_type`.
#
# From the 2026-09-27 doc-diff sweep (`Language/compilation.rakudoc:159`):
# https://github.com/tokuhirom/mutsu/issues/9835

plan 4;

lives-ok {
    EVAL 'role R1 { method m(CompUnit::DependencySpecification $s) {} }; class C1 does R1 { }';
}, 'a role method parameter accepts CompUnit::DependencySpecification';

lives-ok {
    EVAL 'role R2 { method m(Distribution::Resource $s) {} }; class C2 does R2 { }';
}, 'a role method parameter accepts Distribution::Resource';

# The same name already worked outside a role body; pin that it still does.
sub takes-dep-spec(CompUnit::DependencySpecification $s) { $s.^name }
is takes-dep-spec(CompUnit::DependencySpecification), 'CompUnit::DependencySpecification',
    'a sub parameter still accepts CompUnit::DependencySpecification';

# Guard: a genuinely undeclared compound name is still rejected, so the fix did
# not turn the validator into a rubber stamp.
throws-like 'role Bogus { method m(No::Such::Type $x) { $x } }; class BogusC does Bogus { }',
    X::Parameter::InvalidType,
    'an undeclared compound typename is still reported';
