use Test;

# An `is raw` method hands back its container (here a hash element) through
# every way of calling it, the way Method::Protected's `is protected` wrapper
# relies on: `.rw` reports it, a `nextcallee` continuation keeps the
# container, and `++` steps through a code object or a `.&m(...)` call.

plan 11;

class G {
    has %!h = z => 5;
    method it($k) is raw { %!h{$k} }
    method plain($k) { %!h{$k} }
    method h() { %!h<z> }
}

# `is raw` sets a routine's `rw`, as `is rw` does.
{
    sub f() is raw { 1 }
    sub g() is rw { 1 }
    sub n() { 1 }
    is-deeply (&f.rw, &g.rw, &n.rw), (True, True, False), 'Sub.rw: raw, rw, plain';
    is-deeply (G.^lookup('it').rw, G.^lookup('plain').rw), (True, False),
        'Method.rw from ^lookup: raw, plain';
    my $m = my method m() is raw { 1 };
    ok $m.rw, 'an anonymous `is raw` method is rw';
}

# `++` through a Method object held in an `&`-variable.
{
    my $g = G.new;
    my &o = G.^lookup('it');
    o($g, 'z')++;
    is $g.h, 6, 'o($obj, ...)++ steps through a Method object';
    ++o($g, 'z');
    is $g.h, 7, '++o($obj, ...) too';
}

# `++` on `$obj.&m(...)`, whose result is a container.
{
    my $g = G.new;
    my &o = G.^lookup('it');
    my &r = my method (|c) is raw { o(self, |c) };
    $g.&r('z')++;
    is $g.h, 6, '$obj.&m(...)++ steps through the returned container';
    throws-like { my $x = 1; (1 + $x)++ }, X::Multi::NoMatch,
        'an operand with no container still raises';
}

# A wrapper that calls the original through `nextcallee`.
{
    my class W {
        has %!h = z => 5;
        method it($k) is raw { %!h{$k} }
        method h() { %!h<z> }
    }
    W.^lookup('it').wrap: my method (|c) is raw {
        my &original = nextcallee;
        original(self, |c)
    };
    my $w = W.new;
    my $c := $w.it('z');
    $c = 10;
    is $w.h, 10, 'a nextcallee continuation keeps the raw container';
    $w.it('z') = 11;
    is $w.h, 11, 'assignment through the wrapped raw method';
    $w.it('z')++;
    is $w.h, 12, '++ through the wrapped raw method';
}

# The same through a Lock, Method::Protected's exact shape.
{
    my class P {
        has %!h = z => 5;
        has $!lock = Lock.new;
        method lock() { $!lock }
        method it($k) is raw { %!h{$k} }
        method h() { %!h<z> }
    }
    P.^lookup('it').wrap: my method (|c) is raw {
        my &original = nextcallee;
        self.lock.protect: { original(self, |c) }
    };
    my $p = P.new;
    my $value = $p.it('z')++;
    is $p.it('z'), $value + 1, 'a protected raw method can be incremented';
}
