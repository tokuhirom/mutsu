use Test;

plan 5;

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

# 4: a genuine `:=` attribute binding still wins (what the scan is for).
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

# 5: an rw parameter still writes back to its caller.
{
    class E {
        has $.pol;
        method run() { my $pol = 1; self!bump($pol); $pol }
        method !bump($pol is rw) { $pol = 99 }
    }
    is E.new.run, 99, 'the rw parameter still writes back to the caller';
}
