use Test;

plan 5;

# `WALK(method)()` invokes the MRO-level candidates LAZILY — one method call per
# pulled element (Rakudo). `for $obj.WALK("foo")() { ... }` calls exactly one
# candidate per iteration; assigning the result to an `@`-array reifies (eager).

my class C1 {
    has $.calls is rw = 0;
    has @.order;
    method foo { ++$.calls; @.order.push: 'C1' }
}
my class C2 is C1 {
    method foo { ++$.calls; @.order.push: 'C2' }
}
my class C3 is C2 {
    method foo { ++$.calls; @.order.push: 'C3' }
}

# Lazy: the Nth iteration triggers exactly the Nth candidate call.
my $obj = C3.new;
my @seen-counts;
my $step = 0;
for $obj.WALK("foo")() {
    ++$step;
    @seen-counts.push: $obj.calls;
}
is $step, 3, 'WALK("foo")() yields one element per MRO level';
is-deeply @seen-counts, [1, 2, 3], 'each iteration invoked exactly one more candidate';
is-deeply $obj.order, ['C3', 'C2', 'C1'], 'candidates invoked most-derived first';

# Eager reification into an @-array forces all candidates.
my class A { method m { 1 } }
my class B is A { method m { 2 } }
my @r = B.new.WALK("m")();
is @r.elems, 2, 'WALK result reifies to an array eagerly';
is-deeply @r, [2, 1], 'eager WALK collects all candidate results';
