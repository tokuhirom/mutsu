# A bareword type name or constant read inside a class nested in a package
# resolves through the enclosing package chain; a term key (`\Bool`) takes
# the narrow env probe there, not the alias cascade (#18274).
use Test;
module M {
    my constant LIMIT = 3;
    our constant OURC = 7;
    my \sigless = 11;
    class M::C {
        method lim { LIMIT }
        method ourc { OURC }
        method sl { sigless }
        method chk($x) { $x ~~ Int }
        method chk-bool($x) { $x ~~ Bool }
    }
}
module N {
    my constant Bool = Str;
    class N::K {
        method chk($x) { $x ~~ Bool }
    }
}
is M::C.lim, 3, 'package-block constant';
is M::C.ourc, 7, 'our constant';
is M::C.sl, 11, 'sigilless';
ok M::C.chk(5), 'core type check';
ok M::C.chk-bool(True), 'Bool';
nok M::C.chk-bool(5), 'not Bool';
for ^3 { ok M::C.chk-bool(True), "repeat $_" }
ok N::K.chk("x"), "constant shadows Bool in the package chain";
nok N::K.chk(True), 'shadowed Bool is Str';
sub outer(\Bool) {
    my sub inner($x) { $x ~~ Bool }
    inner("s"), inner(1)
}
is-deeply outer(Str).List, (True, False), 'sigilless parameter shadows a type name in a nested sub';
my \Real = Str;
sub r($x) { $x ~~ Real }
for ^3 { ok r("x"), "file-scope sigilless term shadows a core type in a sub ($_)" }
nok r(1.5), 'the shadowing term is Str';
done-testing;
