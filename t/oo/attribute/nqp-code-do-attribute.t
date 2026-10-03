use v6;
use Test;
use nqp;

# #11207: `Code.$!do` is a code object's body. Rebinding it through
# nqp::bindattr changes what every call of the routine runs, while the
# routine object keeps its identity, signature and mixins. Upstream
# NativeCall's backend-neutral path replaces an `is native` routine's body
# this way (ADR-11203).

plan 14;

{
    sub a { "a" }
    my $b := -> { "b" };
    nqp::bindattr(&a, Code, '$!do', nqp::getattr($b, Code, '$!do'));
    is a(), 'b', 'a named call runs the bound body';
    is &a(), 'b', 'a &name call runs the bound body';
    my $c = &a;
    is $c(), 'b', 'a call through a copied code value runs the bound body';
}

{
    sub g($x) { "g$x" }
    my $orig := nqp::getattr(&g, Code, '$!do');
    is $orig(1), 'g1', 'getattr($!do) answers the declared body, callable';
    my $rp := -> |c { "replaced {c.raku}" };
    nqp::bindattr(&g, Code, '$!do', nqp::getattr($rp, Code, '$!do'));
    is g(2), 'replaced \(2)', 'the bound body receives the call\'s arguments';
    is $orig(3), 'g3', 'a $!do taken earlier still runs the old body';
    my $now := nqp::getattr(&g, Code, '$!do');
    is $now(4), 'replaced \(4)', 'getattr($!do) answers the bound body';
    nqp::bindattr(&g, Code, '$!do', $orig);
    is g(5), 'g5', 'binding the declared body back restores it';
}

{
    sub w($x) { "w$x" }
    my $rp := -> |c { "do{c[0]}" };
    nqp::bindattr(&w, Code, '$!do', nqp::getattr($rp, Code, '$!do'));
    &w.wrap(-> |c { "<" ~ callsame() ~ ">" });
    is w(1), '<do1>', 'a wrapper .wrapped after the bind runs around the bound body';
}

{
    # The NativeCall shape: a trait mixes a role into the routine, and a
    # method of that role replaces the routine's body with a closure that
    # sees `self`, the mixed-in routine object. mutsu applies traits at run
    # time, after the routine is compiled, so the replacement stands. Rakudo
    # runs the trait at compile time and finishes compiling the routine
    # afterwards, which undoes the bind on MoarVM (where NativeCall takes its
    # dispatcher path instead).
    my role Replace[$tag] {
        has $.tag = $tag;
        method replace-body() {
            my $replacement := -> |c { "{self.tag}:{c.list.join(',')}" };
            my $do := nqp::getattr($replacement, Code, '$!do');
            # The name is read first: once `$!do` is rebound, `self.name`
            # answers the new body's (empty) name. Upstream NativeCall keeps
            # it in `$!name` for the same reason.
            my $name := self.name;
            nqp::bindattr(self, Code, '$!do', $do);
            nqp::setcodename($do, $name);
        }
    }
    multi trait_mod:<is>(Routine $r, :$replaced!) {
        $r does Replace[$replaced];
        $r.replace-body;
    }
    sub f($a, $b) is replaced<T> { "orig" }
    is f(1, 2), 'T:1,2', 'a body replaced by a trait runs on a named call';
    is &f.tag, 'T', 'the routine keeps its mixed-in role';
    is &f.name, 'f', 'the routine keeps its name';
    my &h = &f;
    is h(3, 4), 'T:3,4', 'a code value of the routine runs the replacement';
    is (f(5, 6) for ^2).join('|'), 'T:5,6|T:5,6', 'repeated calls keep running it';
}
