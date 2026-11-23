# A package-qualified type name written relative to an enclosing package, and
# a qualified core type name, checked from inside a package (#18274).
use Test;
module Core {
    role A::Item { method m(A::Item:D $x) { 'item' } }
    class A::Thing does A::Item { }
    our sub check($v) { $v ~~ A::Item }
    our sub meta($v) { $v.HOW ~~ Metamodel::ClassHOW }
}
my $t = Core::A::Thing.new;
is $t.m($t), 'item', 'a relative compound name in a signature';
ok Core::check($t), 'a relative compound name in a smartmatch';
ok Core::meta($t), 'a core qualified name from inside a package';
for ^3 { ok Core::check($t), "repeated ($_)" }
done-testing;
