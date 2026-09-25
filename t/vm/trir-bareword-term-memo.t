use Test;
use nqp;

# A bareword term in a TRIR routine remembers the type object it resolved to
# for one registry write generation (ADR-0121 D3, #9291). Only an answer the
# name gives by its own spelling is remembered; a name that answers through
# anything else is resolved again on every execution. These pin what that
# must not change. (Each routine here is one TRIR accepts; see
# MUTSU_TRIR_DUMP=1.)

plan 8;

class IB { has $!a; }
class JB { has $!a; }

my sub make(int $n) {
    my int $i = 0;
    nqp::while(nqp::islt_i($i, $n), nqp::stmts(nqp::create(IB), $i = nqp::add_i($i, 1)));
    nqp::create(IB)
}

# The value of a remembered operand is the type object itself.
{
    my $a := make(3);
    my $b := make(3);
    isa-ok $a, IB, 'nqp::create of a bareword class operand builds that class';
    ok $a !=:= $b, 'every create is a fresh object';
}

# A sigilless parameter is a binding, not a bareword: each call creates the
# class it was passed.
my sub make-of(\T) { nqp::create(T) }
isa-ok make-of(IB), IB, 'a sigilless class operand, first class';
isa-ok make-of(JB), JB, 'a sigilless class operand, another class';

# A nested class named by its short spelling inside its outer package
# resolves to the qualified class, which is not the spelling; it keeps
# resolving on every execution.
class Forest {
    class Frog { has $!n }
    our sub frog() { nqp::create(Frog) }
}
is Forest::frog().^name, 'Forest::Frog', 'a short name resolves to the nested class';
is Forest::frog().^name, 'Forest::Frog', 'and still does on a second call';

# A declaration after the routine has run moves the registry generation; the
# answer stays right.
isa-ok make(1), IB, 'before a declaration';
class Later { }
isa-ok make(1), IB, 'after a declaration';
