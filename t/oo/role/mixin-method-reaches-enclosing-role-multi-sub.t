use v6;
use Test;

# From the XML::Class distribution (Map::Mapnik's suite): a method of a nested
# `my role`, mixed into a value with `does`, runs under the synthetic package
# `Any+{R::W}`; a `multi sub` declared in the enclosing role body must still
# be reachable by bare name.

plan 3;

role R[Str :$p] {
    my role W { method add($v) { helper($v) } }
    multi sub helper(Int $v) { "int$v" }
    multi sub helper(Str $v) { "str$v" }
    method go($v) { my $x = Any.new; $x does W; $x.add($v) }
}
class C does R { }
class D does R[p => "f"] { }

is C.new.go(1), 'int1', 'multi sub of a parametric role, composed without arguments';
is D.new.go("a"), 'stra', 'multi sub of a parametric role, composed with arguments';

role Plain {
    my role W { method add($v) { plain-helper($v) } }
    multi sub plain-helper(Int $v) { "int$v" }
    method go($v) { my $x = Any.new; $x does W; $x.add($v) }
}
class E does Plain { }
is E.new.go(2), 'int2', 'multi sub of a non-parametric role';
