use Test;

# A generated accessor call on a variable (`$obj.x`) that has once been
# answered by the attribute's plain read remembers the slot of the
# receiver's layout, and later calls read that slot before any of the
# method-dispatch probes run (ADR-0121 D3). These pin the cases where that
# memory must NOT be replayed, or must follow a change made after it was
# filled.

plan 14;

class P {
    has $.x;
    has $.y is rw;
    has @.list;
    method set-x($v) { $!x = $v }
}

# The same call site sees new values after writes through every route.
{
    my $p = P.new(x => 1, y => 2);
    my @seen;
    for ^3 { @seen.push($p.x); $p.set-x($p.x + 10) }
    is @seen.join(','), '1,11,21', 'repeated reads see writes made by a method';
    $p.y = 5;
    my $s = 0;
    $s += $p.y for ^3;
    is $s, 15, 'an rw accessor read after an assignment through it';
}

# One call site, several layouts: a subclass appends attributes, and a
# sibling class declares the same accessor at a different slot.
{
    class Q is P { has $.q = 'q' }
    class R { has $.pad = 0; has $.x = 'r-x' }
    my @objs = P.new(x => 'p'), Q.new(x => 'q-x'), R.new, P.new(x => 'p2');
    my @got;
    for @objs -> $o { @got.push($o.x) }
    is @got.join(','), 'p,q-x,r-x,p2', 'one call site across alternating layouts';
}

# A subclass overriding the accessor with a method wins, even after the
# parent's accessor was cached for the parent's layout.
{
    class S is P { method x { 'overridden' } }
    my @got;
    for P.new(x => 7), S.new(x => 8), P.new(x => 9) -> $o { @got.push($o.x) }
    is @got.join(','), '7,overridden,9', 'an overriding method is not skipped';
}

# Wrapping the accessor after the call was cached reaches the wrapper.
{
    class W { has $.v = 3 }
    my $w = W.new;
    my $before = 0;
    $before += $w.v for ^3;
    W.^method_table<v>.wrap(-> $self { callsame() * 100 });
    is $before, 9, 'reads before the wrap';
    is $w.v, 300, 'a wrap installed later is honoured';
}

# A method added through the MOP (on a subclass) after the parent's
# accessor was cached for the subclass's layout.
{
    class M { has $.name = 'attr' }
    class M2 is M { }
    my $m = M2.new;
    my $first = $m.name ~ $m.name;
    M2.^add_method('name', method { 'added' });
    M2.^compose;
    is $first, 'attrattr', 'reads before ^add_method';
    is $m.name, 'added', 'a method added later is honoured';
}

# An aggregate attribute is never replayed from the slot: it carries its
# declared container type.
{
    class T { has Int @.nums }
    my $t = T.new(nums => [1, 2]);
    my @r;
    @r.push($t.nums.elems) for ^2;
    is @r.join(','), '2,2', 'an @-attribute accessor read';
    dies-ok { $t.nums.push('str') }, 'and it keeps its element type';
}

# An unset attribute, then set.
{
    class U { has $.maybe; method fill { $!maybe = 'set' } }
    my $u = U.new;
    my @r;
    for ^2 { @r.push($u.maybe // 'undef') }
    $u.fill;
    @r.push($u.maybe);
    is @r.join(','), 'undef,undef,set', 'an unset attribute, then filled';
}

# A runtime role mixin turns the instance into a mixin; the role's own
# accessor must not be answered from the class layout.
{
    role HasX { has $.x = 'role-x' }
    class V { has $.x = 'class-x' }
    my $v = V.new;
    my @r;
    @r.push($v.x) for ^2;
    my $mixed = V.new but HasX;
    @r.push($mixed.x);
    @r.push($v.x);
    is @r.join(','), 'class-x,class-x,role-x,class-x', 'a mixin does not use the class slot';
}

# Private attributes have no accessor, cached or not.
{
    class X1 { has $!secret = 's'; has $.open = 'o' }
    my $o = X1.new;
    my $r = $o.open ~ $o.open;
    is $r, 'oo', 'the public accessor';
    dies-ok { $o.secret }, 'a private attribute still has no accessor';
}
