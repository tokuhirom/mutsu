use Test;

plan 5;

# A `my role` (lexically scoped) declared inside a role body is allowed,
# mirroring how `my class` is permitted inside a role. Previously mutsu
# rejected it with X::Declaration::OurScopeInRole, which blocked DBIish
# (DBDish::StatementHandle declares `my role IntTrue` inside a unit role).
role Outer {
    my role IntTrue { method Bool { self.defined } }
    method wrap($x) { $x but IntTrue }
}
class C does Outer {}

my $v = C.new.wrap(42);
ok $v.Bool, 'mixed-in my role method works';
is $v + 0, 42, 'underlying value preserved through mixin';
isa-ok $v, Int, 'value still an Int';

# An implicitly our-scoped `role` inside a role is still forbidden.
throws-like 'role A { role B { } }', X::Declaration::OurScopeInRole,
    'implicit our-scoped role inside a role is forbidden';

# An explicit `our role` inside a role is still forbidden.
throws-like 'role A { our role B { } }', X::Declaration::OurScopeInRole,
    'explicit our-scoped role inside a role is forbidden';
