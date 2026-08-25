unit module UnitOurScalar;

# Package-scoped (`our`) SCALARS. Their canonical storage is one shared cell
# published under the package-qualified name (`$UnitOurScalar::s`), but this
# module's own routines reference them by the BARE name -- which is exactly
# the resolution the loading script's same-named `my $s` used to hijack, in
# BOTH directions: the module's write landed on the script's lexical, and the
# module's read found the script's value (or `Nil`) instead of its own.
#
# Sibling of t/lib/UnitOurContainer.rakumod; see ADR-0039 sec 4.1.

our $s = 'S';
our $n = 0;

sub s-read()    is export { $s }
sub s-set($v)   is export { $s = $v }
sub s-append()  is export { $s = $s ~ '+' }
sub n-read()    is export { $n }
sub n-inc()     is export { $n++ }
sub n-add($by)  is export { $n += $by }

# A block nested inside a module routine still sees the package scalar.
sub s-set-in-block($v) is export {
    for 1 .. 1 { $s = $v }
    $s
}

# An anonymous closure inside a module routine also reaches the package
# scalar, when the routine declares no same-named lexical of its own.
sub s-set-in-closure($v) is export {
    my $f = { $s = $v };
    $f();
    $s
}

# A routine-local `my $s` SHADOWS the package variable: the lexical
# declaration wins inside this routine, and the package scalar must be left
# completely untouched -- including from a closure nested inside it, which
# captures the routine's lexical, not the package variable.
sub shadowed-local() is export {
    my $s = 'p';
    $s = $s ~ 'q';
    $s
}

sub shadowed-local-closure() is export {
    my $s = 'p';
    my $f = { $s = $s ~ 'q' };
    $f();
    $s
}

# A parameter named like the package scalar shadows it too.
sub shadowed-param($s) is export { $s ~ '!' }

# Interpolation reads the package scalar like any other read.
sub s-interp() is export { "[$s]" }

class Holder is export {
    our $c = 'C';
    method c-read() { $c }
    method c-set($v) { $c = $v }
}

module Deep {
    our $d = 'D';
    our sub d-read() is export { $d }
    our sub d-set($v) is export { $d = $v }
}
