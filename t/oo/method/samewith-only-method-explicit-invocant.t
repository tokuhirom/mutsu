use Test;

# From the UpRooted distribution (UpRooted::Writer::PostgreSQLFile): inside a
# lone (non-multi) method, `samewith` re-calls the routine like a sub, so the
# invocant is spelled out as the first argument. A multi method's dispatcher
# supplies it instead.

plan 4;

class A {
    method o($v, $t?) { $v ~~ Int ?? samewith(self, $v.Str, 1) !! "o$v" }
    method !p($v, $t?) { $v ~~ Array ?? $v.map({ samewith(self, $_, 1) }).join(',') !! "p$v" }
    method pp { self!p([1, 2]) }
    multi method m(Int $x) { samewith $x.Str }
    multi method m(Str $x) { "s$x" }
}

is A.new.o(1), 'o1', 'public only-method samewith with explicit self';
is A.new.pp, 'p1,p2', 'private only-method samewith with explicit self';
is A.new.m(1), 's1', 'multi method samewith keeps the implicit invocant';
is A.new.o("x"), 'ox', 'no recursion needed';
