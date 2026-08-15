use Test;

# A sigilless `constant \NAME` declared directly inside a non-unit
# `module`/`package` block used to be invisible to a nested `sub`: the
# bareword read fell all the way through the compiler's bareword-resolution
# fallback chain to the runtime's ultimate string-fallback, returning the
# bareword's own name as a Str instead of the constant's value. Only
# non-compile-time-foldable initializers (a method call like `blob8.new(...)`,
# not a literal) hit this, since a literal-valued constant already resolves
# via the separate ADR-0006 §2.2 constant-folding path. `class` bodies were
# unaffected (a runtime env-lifecycle asymmetry happened to let the bareword's
# transient env residue survive there), as was a SIGILED `constant $NAME`
# (referenced with its sigil).

plan 6;

module M1 {
    constant \A = blob8.new(1);
    sub s1() is export { A }
}
import M1;
is s1(), Blob[uint8].new(1), 'sigilless constant inside a non-unit module, read from a nested sub';

package P1 {
    constant \B = blob8.new(2);
    our sub s2() is export { B }
}
import P1;
is s2(), Blob[uint8].new(2), 'same shape inside a non-unit package';

module M2 {
    my constant \C = blob8.new(3);
    sub s3() is export { C }
}
import M2;
is s3(), Blob[uint8].new(3), '"my constant" form';

module M3 {
    our constant \D = blob8.new(4);
    sub s4() is export { D }
}
import M3;
is s4(), Blob[uint8].new(4), '"our constant" form';

# class bodies already worked; guard against a regression.
class C1 {
    constant \E = blob8.new(5);
    method m1() { E }
}
is C1.new.m1, Blob[uint8].new(5), 'class body (already-working case) unaffected';

# A sigiled constant, read with its sigil, already worked; guard against a
# regression from the new package-chain bareword fallback.
module M4 {
    constant $F = blob8.new(6);
    sub s6() is export { $F }
}
import M4;
is s6(), Blob[uint8].new(6), 'sigiled constant read with its sigil still works';

done-testing;
