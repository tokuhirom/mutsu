use Test;

# A closure created on one thread and called on another runs with its
# creator's routines installed for the call (#18135). Calling it once per
# loop iteration must keep every name answering for the side it is resolved
# on, however often the worker switches between the two (#14812: the switch
# reuses the memo generation it was given last time instead of retiring
# every memo on each call).

plan 5;

sub label { 'main' }
my &main-label = { label() };

class Counter {
    has $.n = 0;
    method bump { $!n++ }
}
my &main-bump = -> $c { $c.bump; $c.n };

my @seen = await start {
    my sub label { 'worker' }
    my $c = Counter.new;
    my @out;
    for ^4 {
        @out.push: label() ~ '/' ~ main-label() ~ '/' ~ main-bump($c) ~ '/' ~ label();
    }
    @out
};
is-deeply @seen.List,
    ('worker/main/1/worker', 'worker/main/2/worker', 'worker/main/3/worker', 'worker/main/4/worker'),
    'each side keeps its own routine, call after call';

# A call that declares something (a write while the creator's routines are
# installed) is followed by calls that still see the right routines.
my &main-declares = { EVAL 'class LateFromClosure { method hi { "late" } }'; label() };
my @after = await start {
    my sub label { 'worker' }
    my @out;
    @out.push: main-declares();
    @out.push: label();
    @out.push: main-label();
    @out.push: ::('LateFromClosure').hi;
    @out
};
is @after[0], 'main', 'the declaring call resolves its creator\'s routine';
is @after[1], 'worker', 'after it, the worker\'s own routine';
is @after[2], 'main', 'and the next closure call its creator\'s again';
is @after[3], 'late', 'a class declared during the call is visible afterwards';
