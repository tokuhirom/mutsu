use Test;

plan 7;

# An `is rw`/`is raw` scalar parameter binds through a shared `ContainerRef`
# cell. The method-exit attribute reconcile scans each attribute's bare name for
# such a cell to recover a `:=` attribute binding (`$!x := $outer`) — and used to
# find the *parameter* (or a caller variable of the same name, reachable because
# a callee's env is the flattened caller env) and adopt it as the attribute's new
# value, permanently replacing the attribute.

class P { has $.total }

# 1-2: the frame's own parameter shares the attribute's name.
{
    class A {
        has P $.pol = P.new(total => 7);
        method run() { my P $q; self!fill($q); }
        method !fill(P $pol is rw) { }
    }
    my $a = A.new;
    $a.run;
    is $a.pol.total, 7, 'a same-named rw parameter does not replace the attribute';
    $a.run;
    is $a.pol.total, 7, 'and still not on a second call';
}

# 3: a caller variable shares the attribute's name.
{
    class B {
        has P $.pol;
        method run() {
            my P $pol;
            self!fill($pol);
            return $!pol.defined;
        }
        method !fill(P $z is rw) { }
    }
    nok B.new.run, "a caller's same-named variable does not replace the attribute";
}

# 4-5: the frame's own `my` lexical shares the attribute's name and is boxed
# into a cell by being passed to an rw parameter. This is the shape
# `Cro::HTTP::Client.request`'s `my Cro::Policy::Timeout $timeout-policy` has
# against its own `has $.timeout-policy`; reading the attribute in the callee is
# what made the adopted cell observable.
{
    class F {
        has P $.pol;
        method go() { my P $pol; self!fill($pol); 'ok' }
        method !fill(P $pol is rw) {
            my $d = P.new(total => 1);
            ($pol = $!pol // $d) without $pol;
        }
    }
    my $f = F.new;
    is (try $f.go) // $!.message, 'ok', "a frame's own my-lexical does not replace the attribute";
    is (try $f.go) // $!.message, 'ok', 'and the second call on the same instance still works';
}

# 6: a genuine `:=` attribute binding still wins (what the scan is for).
{
    class D {
        has $.bound;
        method bind-to($x is rw) { $!bound := $x; }
        method peek() { $!bound }
    }
    my $outer = 1;
    my $d = D.new;
    $d.bind-to($outer);
    $outer = 42;
    is $d.peek, 42, 'a real := attribute binding still tracks its target';
}

# 7: an rw parameter still writes back to its caller.
{
    class E {
        has $.pol;
        method run() { my $pol = 1; self!bump($pol); $pol }
        method !bump($pol is rw) { $pol = 99 }
    }
    is E.new.run, 99, 'the rw parameter still writes back to the caller';
}
