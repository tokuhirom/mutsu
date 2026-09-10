use Test;

# Entities defined in a class are prioritized over entities from a composed
# role: a role method must not shadow the class's own attribute accessor of the
# same name, and a class method overrides a role's attribute accessor.
# (6.c S14-roles/attributes.t "Class prioritization".)

plan 6;

my role R {
    has $.r1 = 100;
    method c1 { 666 }
}
my class C does R {
    has $.c1 = 42;
    method r1 { 7 }
}

my $inst = C.new;

# accessor (class) beats method (role), through a variable holder ...
is $inst.c1, 42, 'class attribute accessor wins over role method (var form)';
# ... and through a chained call (different dispatch path)
is C.new.c1, 42, 'class attribute accessor wins over role method (chained form)';

# method (class) beats accessor (role)
is $inst.r1, 7, 'class method wins over role attribute accessor (var form)';
is C.new.r1, 7, 'class method wins over role attribute accessor (chained form)';

# A role method with NO competing class entity still dispatches normally.
my role R2 { method only-role { 'from-role' } }
my class C2 does R2 { }
is C2.new.only-role, 'from-role', 'role method still runs when unshadowed';

# A role accessor with no competing class entity still works.
my role R3 { has $.rx = 'roleattr' }
my class C3 does R3 { }
is C3.new.rx, 'roleattr', 'role attribute accessor still runs when unshadowed';
