use v6;
use Test;

# #12773: a nested `my role W` is lexical to its declaring role body, so two roles
# each declaring one keep their own `W` after both are composed.

plan 4;

role RA {
    my role W { method n { "RA" } }
    method go { my $x = Any.new; $x does W; $x.n }
}
role RB {
    my role W { method n { "RB" } }
    method go { my $x = Any.new; $x does W; $x.n }
}
class DA does RA { }
class DB does RB { }
is DA.new.go, 'RA', 'first role keeps its nested W';
is DB.new.go, 'RB', 'second role keeps its nested W';

role R {
    my role W { method add($v) { helper($v) } }
    multi sub helper(Int $v) { "int$v" }
    multi sub helper(Str $v) { "str$v" }
    method go($v) { my $x = Any.new; $x does W; $x.add($v) }
}
class C does R { }
role Plain {
    my role W { method add($v) { plain-helper($v) } }
    multi sub plain-helper(Int $v) { "int$v" }
    method go($v) { my $x = Any.new; $x does W; $x.add($v) }
}
class E does Plain { }
is C.new.go("a"), 'stra', 'nested W reaches its own role multi sub';
is E.new.go(3), 'int3', 'second role nested W reaches its own multi sub';
