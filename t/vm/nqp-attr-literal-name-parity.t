use Test;
use nqp;

# An `nqp::getattr` / `bindattr` / `p6bindattrinvres` site whose name operand is
# a string literal resolves the name when it is compiled (ADR-0121 D3,
# `OpCode::NqpAttrC`). A site whose name is computed keeps the generic op.
# Every answer here is pinned for both spellings, so the two cannot drift.

plan 19;

class P {
    has int $!n;
    has num $!f;
    has str $!s;
    has $!plain;
    has @!items;
}

my $name = '$!plain';

# The value a bind hands back, and the converted value the typed forms store.
{
    my $p := nqp::create(P);
    is nqp::bindattr($p, P, '$!plain', 42), 42, 'literal bindattr returns the value';
    is nqp::bindattr($p, P, $name, 43), 43, 'computed bindattr returns the value';
    is nqp::getattr($p, P, '$!plain'), 43, 'the literal read sees the computed bind';
    is nqp::getattr($p, P, $name), 43, 'the computed read agrees';

    is nqp::bindattr_i($p, P, '$!n', 7), 7, 'bindattr_i returns the int';
    is nqp::getattr_i($p, P, '$!n'), 7, 'getattr_i reads it back';
    is nqp::bindattr_n($p, P, '$!f', 1e0), 1e0, 'bindattr_n returns the num';
    isa-ok nqp::getattr_n($p, P, '$!f'), Num, 'getattr_n reads a Num';
    is nqp::bindattr_s($p, P, '$!s', '12'), '12', 'bindattr_s returns the str';
    isa-ok nqp::getattr_s($p, P, '$!s'), Str, 'getattr_s reads a Str';
}

# p6bindattrinvres hands back the invocant.
{
    my $p := nqp::create(P);
    my $r := nqp::p6bindattrinvres($p, P, '$!plain', 'x');
    ok nqp::eqaddr($r, $p), 'p6bindattrinvres returns the invocant';
    is nqp::getattr($p, P, '$!plain'), 'x', 'and bound the attribute';
}

# The class operand is still evaluated, once per execution.
{
    my $calls = 0;
    sub cls { $calls++; P }
    my $p := nqp::create(P);
    nqp::bindattr($p, cls(), '$!plain', 1);
    nqp::getattr($p, cls(), '$!plain');
    is $calls, 2, 'the class operand runs once per execution';
}

# The same site, run in a loop, on instances of two unrelated classes.
{
    class Q is P { has $!other }
    my @objs = nqp::create(P), nqp::create(Q), nqp::create(P);
    my $i = 0;
    for @objs -> $o {
        nqp::bindattr(nqp::decont($o), P, '$!plain', $i++);
    }
    is @objs.map({ nqp::getattr(nqp::decont($_), P, '$!plain') }).join(','), '0,1,2',
        'one site serves a class and its subclass';
}

# A container attribute named by its sigil.
{
    my $p := nqp::create(P);
    nqp::bindattr($p, P, '@!items', [1, 2, 3]);
    is nqp::getattr($p, P, '@!items').elems, 3, 'a sigiled literal finds the attribute';
}

# Non-instance receivers keep the generic behaviour.
{
    my %h = a => 1;
    nqp::bindkey(nqp::getattr(%h, Map, '$!storage'), 'b', 2);
    is %h<b>, 2, "a Map's literal \$!storage is the hash itself";
    my $pair := nqp::decont('k' => 1);
    nqp::bindattr($pair, Pair, '$!value', 5);
    is $pair.value, 5, "a Pair's literal \$!value binds";
}

# An empty attribute name still fails loudly.
throws-like { nqp::bindattr(nqp::create(P), P, '$!', 1) }, Exception,
    'a literal name that strips to nothing is an error';

# Inside a method, on self.
class R {
    has $!x = 1;
    method bump { nqp::bindattr(self, R, '$!x', nqp::getattr(self, R, '$!x') + 1) }
}
is R.new.bump, 2, 'literal getattr/bindattr on self inside a method';
