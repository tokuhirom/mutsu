use v6;
use Test;

# A closure that captured an `@`/`%` PARAMETER of its defining routine, and
# that itself spawns a `start` block the block never names the parameter in,
# must keep writing into the caller's container after the spawn. The spawn
# used to seed the parameter into the name-keyed cross-thread store (as a
# transient entry), and the next `%g{$k} = ...` then took the atomic hash
# lane, which writes a copy and rebinds `%g` to it -- so every store after
# the first spawn was lost to the caller (#10031). Regression pin for the
# JobQueue distribution's t/02-coordinator.rakutest, whose test hung waiting
# on gates the second queue's runner had registered into the detached copy.

plan 4;

# The issue's reduction: two closures over the same caller hash.
{
    sub mk(%g) { -> $id { %g{$id} = 1; start { 1 } } }
    my %gates;
    my &f = mk(%gates);
    my &h = mk(%gates);
    f('a');
    h('b');
    f('c');
    await Promise.in(0.05);
    is %gates.keys.sort.join(','), 'a,b,c',
        'every store through a spawning closure reaches the caller hash';
}

# The same shape for an array parameter.
{
    sub mk-arr(@g) { -> $v { @g[$v] = $v; start { 1 } } }
    my @seen;
    my &f = mk-arr(@seen);
    my &h = mk-arr(@seen);
    f(0);
    h(1);
    f(2);
    await Promise.in(0.05);
    is @seen.join(','), '0,1,2',
        'every element store through a spawning closure reaches the caller array';
}

# The JobQueue shape: the closure is stored in an object attribute, invoked
# through a method, and its spawned block awaits a promise it registered.
{
    class Runner {
        has &.run is required;
        method go($k) { &!run($k) }
    }
    sub mk-runner(%gates) {
        Runner.new(run => -> $k {
            %gates{$k} = my $g = Promise.new;
            start { await $g }
        })
    }
    my %gates;
    my $a = mk-runner(%gates);
    my $b = mk-runner(%gates);
    my @done = $a.go('x'), $a.go('y'), $b.go('z');
    is %gates.keys.sort.join(','), 'x,y,z', 'all gates are visible to the caller';
    .keep('done') for %gates.values;
    await Promise.anyof(Promise.allof(@done), Promise.in(10));
    is @done.map(*.status).join(','), 'Kept,Kept,Kept', 'keeping the caller-visible gates releases every runner';
}

