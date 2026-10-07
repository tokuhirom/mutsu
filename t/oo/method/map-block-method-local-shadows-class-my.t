use Test;

# #11718: a method-local `my` that the method WRITES, read by an inline
# .map/.grep block, is lexically nearer than the class body's same-named `my`.

plan 8;

class V {
    my $enc = "outer";
    method b() { my $enc = "inner"; $enc ~= "!"; (1,).map({ $enc }).join }
    method c() { my $enc = "inner"; $enc ~= "!"; my $f = { $enc }; $f() }
    method d() { my $enc = "inner"; $enc ~= "!"; (1,).grep({ $enc eq "inner!" }).elems }
    method e() {
        my $enc = "inner"; $enc ~= "!";
        my @r;
        for 1, 2 { @r.push: (1,).map({ $enc }).join }
        @r.join(",")
    }
    method f() { my $enc = "inner"; $enc ~= "!"; (1,).map({ (2,).map({ $enc }).join }).join }
    method h() { my @enc = <a b>; @enc.push("c"); (1,).map({ @enc.join }).join }
    method never() { my $enc = "inner"; (1,).map({ $enc }).join }
}

is V.new.b, "inner!", "written method-local read by map block";
is V.new.c, "inner!", "plain closure";
is V.new.d, 1, "grep block";
is V.new.e, "inner!,inner!", "map inside a for loop";
is V.new.f, "inner!", "nested map blocks";
is V.new.h, "abc", "written method-local array";
is V.new.never, "inner", "never-written method-local";

package P {
    my $x = "pk";
    our sub f { my $x = "in"; $x ~= "!"; (1,).map({ $x }).join }
}
is P::f(), "in!", "package sub's written local";
