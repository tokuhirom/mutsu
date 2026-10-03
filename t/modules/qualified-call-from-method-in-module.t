use lib 't/lib';
use Test;
use QualifiedCallFromMethod;

# `Inner::f` inside a method of a class or role declared in a module names the
# module's own `our package Inner`, found through the enclosing package, as
# Rakudo finds it through the lexical scope. It used to resolve only from the
# module's own subs: a method died with "Could not find symbol '&f' in
# 'GLOBAL::Inner'" for a sub and "Cannot resolve caller" for a multi (Bitcoin's
# `P2PKH::address self` from `role Bitcoin::PrivateKey`).

plan 7;

is QualifiedCallFromMethod::from-sub(), 'str:sub:False', 'from a module sub (already worked)';
is QualifiedCallFromMethod::C.plain, 'plain:c', 'an our sub from a method';
is QualifiedCallFromMethod::C.multi, 'uint:7:True', 'a proto/multi from a method';
is QualifiedCallFromMethod::C.in-closure, 'plain:closure', 'from a closure inside a method';
is QualifiedCallFromMethod::C.via-sub, 'plain:lexical-sub', 'from a lexical sub of the class';
is (5 but QualifiedCallFromMethod::R).from-role, 'uint:5:False', 'from a role method on a mixin';

throws-like { QualifiedCallFromMethod::C.^find_method('plain'); NoSuchPkg::zz(1) }, Exception,
    'an unknown qualifier still dies';
