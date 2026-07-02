use Test;

# Postconstraints on `my (...)` destructuring targets (S12-subset/subtypes.t 90):
#  - a bare literal element (`my ("foo")`) is a constraint the assigned value must
#    match, else X::TypeCheck::Assignment;
#  - the enforcement holds in value position and with a preceding sigilless element;
#  - and `(my (...) = list)` yields the assigned list, not just the last element.

plan 9;

throws-like 'my ("foo") = "bar"', X::TypeCheck::Assignment,
    'single bare-literal target rejects a non-matching value';
lives-ok { my ("foo") = "foo" }, 'single bare-literal target accepts a match';

throws-like 'my ($a, "foo") = 1, "bar"', X::TypeCheck::Assignment,
    'literal target after a plain var is enforced';

# A sigilless element before the literal must not disable the literal check
# (the trailing-result fix keeps the constrained decl off block-final position).
throws-like 'my (\b where "x", "foo") = "x", "bar"', X::TypeCheck::Assignment,
    'literal enforced even after a matching sigilless where-element';
throws-like 'my (\b, $c where 5) = "x", 9', X::TypeCheck::Assignment,
    'a where element after a sigilless element is enforced';

# The where-literal / where-type single-arg cases keep working.
throws-like 'my ($a where 2) = 3',   X::TypeCheck::Assignment, 'where literal';
throws-like 'my ($b where Int) = .1', X::TypeCheck::Assignment, 'where type';

# `(my (...) = LIST)` in expression context yields the assigned list.
{
    my $r = (my ($x, $y) = 1, 2);
    is $r.elems, 2, 'destructuring assignment yields the full list (elems)';
    is $r.join(','), '1,2', 'destructuring assignment yields the full list (values)';
}
