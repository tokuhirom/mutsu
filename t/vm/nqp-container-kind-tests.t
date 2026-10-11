use v6;
use Test;
use nqp;

# The container tests `nqp::iscont_i` / `_n` / `_s` and `nqp::isrwcont` ask
# about the operand's binding: its native kind and whether it is writable
# (ADR-11553 §3.2, #12673). Every expected answer was checked against Rakudo
# 2026.09.
plan 44;

# -- native kind --
{
    my int $i = 1;
    my num $n = 1e0;
    my str $s = 'a';
    my $x = 1;
    is nqp::iscont_i($i), 1, 'iscont_i: a native int lexical';
    is nqp::iscont_n($n), 1, 'iscont_n: a native num lexical';
    is nqp::iscont_s($s), 1, 'iscont_s: a native str lexical';
    is nqp::iscont_n($i), 0, 'iscont_n: an int lexical is not a num container';
    is nqp::iscont_i($x), 0, 'iscont_i: a Scalar is not a native container';
    is nqp::iscont_i(1), 0, 'iscont_i: a literal is no container';
    is nqp::iscont($i), 1, 'iscont: a native lexical is a container';
}
{
    my class A {
        has int $.a;
        has str $.b;
        method t { (nqp::iscont_i($!a), nqp::iscont_s($!b), nqp::iscont_i($!b)) }
    }
    is-deeply A.new.t, (1, 1, 0), 'native attributes are native containers of their kind';
}
{
    sub ro(int $i) { (nqp::iscont_i($i), nqp::iscont($i), nqp::isrwcont($i)) }
    is-deeply ro(1), (0, 0, 0), 'a plain native parameter binds a value';
    my int $v = 1;
    sub rw(int $i is rw) { (nqp::iscont_i($i), nqp::isrwcont($i)) }
    is-deeply rw($v), (1, 1), 'a native is rw parameter is a native container';
    is-deeply (-> int $i { nqp::iscont_i($i) })(1), 0, 'a native pointy-block parameter binds a value';
}
{
    my int @n = 1, 2;
    is nqp::iscont_i(@n[0]), 1, 'iscont_i: an element of a native int array';
    is nqp::isrwcont(@n[0]), 1, 'isrwcont: an element of a native int array';
    is nqp::iscont_i(nqp::atposref_i(nqp::list_i(1), 0)), 1, 'iscont_i: an atposref_i result';
    is nqp::iscont_i(nqp::atpos_i(nqp::list_i(1), 0)), 0, 'iscont_i: an atpos_i result is a value';
    my $r := nqp::atposref_i(nqp::list_i(5), 0);
    is nqp::iscont_i($r), 1, 'a native reference bound to a name stays one';
    is nqp::isrwcont($r), 1, '... and is writable';
    sub g(\x) { nqp::iscont_i(x) }
    is g($r), 1, '... through a sigilless parameter too';
    my $l := nqp::list_i(1, 2);
    my $ref := nqp::atposref_i($l, 1);
    $ref = 42;
    is nqp::atpos_i($l, 1), 42, 'a write through the reference reaches the list';
}

# -- writability --
{
    my $x;
    is nqp::isrwcont($x), 1, 'isrwcont: a variable';
    is nqp::isrwcont(1), 0, 'isrwcont: a literal';
    my $v = 1;
    sub rw($p is rw) { nqp::isrwcont($p) }
    sub ro($p) { nqp::isrwcont($p) }
    sub cp($p is copy) { nqp::isrwcont($p) }
    sub raw($p is raw) { nqp::isrwcont($p) }
    is rw($v), 1, 'an is rw parameter';
    is ro($v), 0, 'a read-only parameter over a writable container';
    is cp(1), 1, 'an is copy parameter';
    is raw($v), 1, 'an is raw parameter bound to a variable';
    is raw(1), 0, 'an is raw parameter bound to a literal';
    my @a = 1;
    my %h = a => 1;
    is nqp::isrwcont(@a[0]), 1, 'an Array element';
    is nqp::isrwcont(%h<a>), 1, 'a Hash element';
    my $l = (1, 2);
    is nqp::isrwcont($l[0]), 0, 'a List element is no container';
    my $b := 5;
    is nqp::isrwcont($b), 0, 'a name bound to a literal';
    constant C = 1;
    is nqp::isrwcont(C), 0, 'a constant';
    my $p := Proxy.new(FETCH => { 1 }, STORE => -> $, $ {});
    is nqp::isrwcont($p), 1, 'a Proxy';
    is nqp::iscont($p), 1, 'iscont: a Proxy';
    is nqp::isrwcont(nqp::decont($x)), 0, 'a decontainerized value';
}
{
    my class B { has $.a; method t { nqp::isrwcont($!a) } }
    is B.new.t, 1, 'an attribute';
}
{
    my int $i = 1;
    my $x;
    is nqp::iscont_i($i.VAR), 0, 'an explicit .VAR is a container object, not a native container';
    is nqp::isrwcont($x.VAR), 0, '... and not a writable binding';
    is nqp::iscont($x.VAR), 1, '... but iscont says it is a container';
}

# -- the result of an is raw / is rw routine (#12712) --
{
    my $x = 1;
    sub r() is raw { $x }
    sub rw() is rw { $x }
    sub pl() { $x }
    is nqp::isrwcont(r()), 1, 'an is raw routine result is a writable container';
    is nqp::iscont(r()), 1, '... and a container';
    is nqp::isrwcont(rw()), 1, 'an is rw routine result is a writable container';
    is nqp::iscont(rw()), 1, '... and a container';
    is nqp::isrwcont(pl()), 0, 'a plain routine result is a value';
    is nqp::iscont(pl()), 0, '... not a container';
}
