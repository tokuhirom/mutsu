use Test;

# A role parameter list may end in a trailing comma, and a typed named
# parameter's default must survive it. Reduced from the BTree distribution
# (`role BTree[::ValueType = Any, BTree::Renderer :$Str-renderer = C,]`),
# where the stub `role BTree {...}` made the type `BTree::Renderer` look like
# the `Type ::Capture` form and the parameter was silently dropped.

plan 7;

role K {...}
role K::R { }
class C does K::R { }

role Trailing[::T = Any, K::R :$r = C,] {
    method r { $r }
}
class I1 does Trailing[Int] {}
is I1.new.r.^name, 'C', 'typed named default survives a trailing comma';

role Multi[
    ::ValueType = Any,
    K::R :$gist = C,
    K::R :$str  = C,
] {
    method both { ($gist, $str).map(*.^name).join(',') }
}
class I2 does Multi[Int] {}
is I2.new.both, 'C,C', 'multi-line list with a trailing comma keeps every default';

role NoComma[::T = Any, K::R :$r = C] {
    method r { $r }
}
class I3 does NoComma[Int] {}
is I3.new.r.^name, 'C', 'same list without a trailing comma';

role CaptureLast[K::R :$r = C, ::T = Int,] {
    method pair { ($r.^name, T.^name).join(',') }
}
class I4 does CaptureLast {}
is I4.new.pair, 'C,Int', 'trailing comma after a type-capture default';

# `= my role {...}` defaults take the per-part fallback parser; a qualified
# type whose first segment is a declared role must not be dropped there.
role Fallback[K::R :$r = C, ::T = my role { }] {
    method r { $r }
}
class I5 does Fallback {}
is I5.new.r.^name, 'C', 'qualified typed named param survives the fallback parser';

role Given[::T = Any, K::R :$r = C,] {
    method r { $r }
}
class D does K::R { }
class I6 does Given[Int, :r(D)] {}
is I6.new.r.^name, 'D', 'an explicit named argument still overrides the default';

sub f($a, $b,) { $a + $b }
is f(1, 2), 3, 'routine signature trailing comma is unaffected';
