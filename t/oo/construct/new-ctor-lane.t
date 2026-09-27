use v6;
use Test;

# ADR-0121 D3: `Class.new(named...)` on a plain class skips the dispatch
# probe chain once a call has walked it into the native default constructor
# (`src/vm/vm_ctor_lane.rs`). Every construction below runs in a loop, so the
# later iterations take the lane; each one must still answer exactly what the
# full dispatch answers, including after the class changes shape.

plan 11;

class P { has $.x; has $.y = 10 }

{
    my @made;
    for ^5 -> $i { @made.push: P.new(x => $i, y => $i * 2) }
    is @made.map(*.x).join(','), '0,1,2,3,4', 'repeated construction keeps each named value';
    is @made.map(*.y).join(','), '0,2,4,6,8', 'a second named argument is bound on every call';
}

{
    my @made;
    for ^3 -> $i { @made.push: P.new(x => $i) }
    is @made.map(*.y).join(','), '10,10,10', 'an omitted attribute takes its default on every call';
}

{
    my $seen = 0;
    class Q { has $.v = $seen }
    my @vals;
    for ^3 -> $i { $seen = $i * 5; @vals.push: Q.new.v }
    is @vals.join(','), '0,5,10', 'a default expression reads the current caller lexical';
}

{
    my @made;
    for ^3 { @made.push: P.new(x => 1, bogus => 2) }
    is @made.map({ .^attributes.elems }).join(','), '2,2,2',
        'an undeclared named argument is ignored on every call';
}

{
    for ^3 { P.new(x => 1) }
    throws-like { P.new(1) }, X::Constructor::Positional,
        'a positional argument still dies after the lane is warm';
}

{
    for ^3 { P.new(x => 1) }
    my $j = P.new(x => 1|2).x;
    isa-ok $j, Junction, 'a junction named argument is stored, not autothreaded';
}

{
    class Base { has $.a }
    class Kid is Base { has $.b }
    for ^3 { Base.new(a => 1) }
    my @k;
    for ^3 -> $i { @k.push: Kid.new(a => $i, b => $i + 1) }
    is @k.map({ .a ~ '/' ~ .b }).join(','), '0/1,1/2,2/3', 'a subclass constructs its own shape';
    isa-ok @k[0], Kid, 'the subclass instance has the subclass type';
}

{
    class R { has $.n }
    my @before;
    for ^3 -> $i { @before.push: R.new(n => $i).n }
    R.^add_method('new', method (*%h) { self.bless(n => 'custom') });
    R.^compose;
    my @after;
    for ^3 -> $i { @after.push: R.new(n => $i).n }
    is @before.join(','), '0,1,2', 'the default constructor before a method is added';
    is @after.join(','), 'custom,custom,custom', 'an added `new` method wins after the lane was warm';
}
