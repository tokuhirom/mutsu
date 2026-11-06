use Test;
use nqp;

plan 3;

# An attribute bound with `nqp::bindattr` is not replaced, on method exit, by a
# same-named lexical declared inside a call's argument list (`my $stats := ...`).
my class St { }
my class C {
    has $!stats;
    submethod BUILD() { nqp::bindattr(self, C, '$!stats', nqp::create(IterationBuffer)); }
    method upd() { nqp::push($!stats, my $stats := St.new); 1 }
    method go() { self.upd; $!stats.^name }
    method count() { $!stats.elems }
}

my $c = C.new;
is $c.go, 'IterationBuffer', 'the attribute keeps its buffer after the call';
$c.upd;
is $c.count, 2, 'a second push reaches the same buffer';
is $c.go, 'IterationBuffer', 'still the buffer afterwards';

# vim: expandtab shiftwidth=4
