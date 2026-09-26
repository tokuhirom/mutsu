use Test;
use nqp;

# `nqp::create(Bareword)` outside TRIR compiles to a site that resolves the
# bareword once per registry write generation, and every `create` / `CREATE`
# remembers per class which allocation the type gets and the slot template it
# starts from (ADR-0121 D3, #9291). These pin that the remembered answers are
# the ones a fresh resolution gives, including after a declaration changes
# them. Everything here runs at mainline, on the untyped VM.

plan 16;

class IB { has $!a; has int $!n; method a { $!a }; method n { $!n } }

my @made;
for ^3 { @made.push: nqp::create(IB) }
isa-ok @made[0], IB, 'a bareword class operand builds that class';
ok @made[0] !=:= @made[1], 'every create is a fresh object';
nok @made[2].a.defined, 'an object attribute starts unset';
is @made[2].n, 0, 'a native int attribute starts at 0';

# The slot template is copied, not shared: binding into one instance leaves
# the next one untouched.
nqp::bindattr(@made[0], IB, '$!a', 42);
is nqp::getattr(nqp::create(IB), IB, '$!a').defined, False,
    'a bind into one instance does not reach the next one created';

# `.CREATE` builds from the same remembered shape.
my $c = IB.CREATE;
isa-ok $c, IB, '.CREATE builds the class';
nok $c.a.defined, '.CREATE leaves its attributes unset';

# A declaration at run time moves the registry generation; the answers
# stay right, for the old class and for the new one.
class Grows { has $!a; method a { $!a } }
my $before := nqp::create(Grows);
EVAL 'class Grows2 is Grows { has $.b; }; 1';
my $after := nqp::create(Grows);
isa-ok $after, Grows, 'the class still creates after a run-time declaration';
my $sub := nqp::create(::('Grows2'));
ok $sub.^can('b') && !$sub.b.defined, 'the new subclass creates with its own attribute unset';

# The types that are not plain instances.
my $u := nqp::create(Uni);
nqp::push_i($u, 66);
is nqp::strfromcodes($u), 'B', 'Uni comes back as an empty codepoint store';
my $b := nqp::create(IterationBuffer);
nqp::push($b, 1);
nqp::push($b, 2);
is nqp::elems($b), 2, 'an IterationBuffer comes back empty and usable';

# An `is Hash` subclass gets its backing store even without `new`.
class HSub is Hash { }
my $hs := nqp::create(HSub);
isa-ok $hs, HSub, 'an is-Hash subclass is created';
$hs<x> = 5;
is $hs<x>, 5, 'and its backing store is writable';

# A non-bareword operand takes the generic op.
my $t = IB;
isa-ok nqp::create(nqp::decont($t)), IB, 'a class held in a variable';
my $of-instance := nqp::create(nqp::decont($c));
isa-ok $of-instance, IB, 'an instance operand creates its class';
ok $of-instance !=:= $c, 'as a fresh instance';
