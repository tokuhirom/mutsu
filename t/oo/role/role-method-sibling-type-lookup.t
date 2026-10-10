use Test;

# From the Tinky distribution: a role method composed into a GLOBAL class
# resolves bare type names against the role's enclosing module.
plan 3;

module M {
    role Bef {}
    multi sub trait_mod:<is>(Method $m, :$before-it!) is export { $m does Bef }
    role Obj is export {
        method found { self.^methods.grep(Bef).elems }
        method named { Bef.^name }
    }
}
import M;

my class B does Obj {
    has Str $.seen;
    method bm($x) is before-it { $!seen = $x }
}

is B.new.found, 1, 'bare sibling role type matches in grep';
is B.new.named, 'M::Bef', 'bare name resolves to the sibling role';
ok B.^methods.grep(M::Bef).elems == 1, 'qualified form agrees';
