use nqp;
use Test;

# A `$!attr` owns a Scalar unless a `:=` seated a plain value in it, and that
# decision belongs to the instance: a bind in one method is what `=:=` and
# `nqp::eqaddr` see in another. The nqp-style iterator idiom
# (`$!next := $iter.pull-one` ... `nqp::eqaddr($!next, IterationEnd)`) spins
# forever without it -- the snip and span distributions' suites hung, and
# Iter::Able's group-conseq called its transformer on IterationEnd. Every
# expected value was measured on rakudo.

plan 12;

my class Probe {
    has $!n;
    has $!u;
    method assign() { $!n = IterationEnd }
    method bind()   { $!n := IterationEnd }
    method bind-var() { my $v = IterationEnd; $!n := $v }
    method eqaddr() { nqp::eqaddr($!n, IterationEnd) }
    method same()   { $!n =:= IterationEnd }
    method unset()  { nqp::eqaddr($!u, Any) }
}

{
    my $p = Probe.new;
    $p.bind;
    is $p.eqaddr, 1, 'a value bound in another method is that value (nqp::eqaddr)';
    ok $p.same, 'a value bound in another method is that value (=:=)';
    is $p.unset, 0, 'an untouched attribute is still its Scalar';
}

{
    my $p = Probe.new;
    $p.assign;
    is $p.eqaddr, 0, 'an assigned value sits in the attribute Scalar (nqp::eqaddr)';
    nok $p.same, 'an assigned value sits in the attribute Scalar (=:=)';
}

{
    my $p = Probe.new;
    $p.bind;
    $p.bind-var;
    is $p.eqaddr, 0, 'binding a variable seats its Scalar again';
}

{
    my $a = Probe.new;
    my $b = Probe.new;
    $a.bind;
    $b.assign;
    is $a.eqaddr, 1, 'the bind is per instance (bound one)';
    is $b.eqaddr, 0, 'the bind is per instance (assigned one)';
}

# The iterator shape the snip distribution uses.
my class Chunks does Iterator {
    has $!iterator;
    has $!next;
    method new($iterator) { nqp::create(self)!SET-SELF($iterator) }
    method !SET-SELF($iterator) {
        $!iterator := $iterator;
        $!next := $iterator.pull-one;
        self
    }
    method pull-one() {
        if nqp::eqaddr($!next, IterationEnd) {
            IterationEnd
        }
        else {
            my $got := $!next;
            $!next := $!iterator.pull-one;
            $got
        }
    }
}

{
    my $it = Chunks.new((1, 2).iterator);
    is $it.pull-one, 1, 'first value';
    is $it.pull-one, 2, 'second value';
    ok $it.pull-one =:= IterationEnd, 'the bound IterationEnd ends the iteration';
    is-deeply Seq.new(Chunks.new((3, 4, 5).iterator)).List, (3, 4, 5),
      'a Seq over the iterator terminates';
}
