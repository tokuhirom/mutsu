use Test;

# A multi family's dispatch caches are validated by the family's own
# generation, not only by the whole routine registry's (ADR-12529 §2.4): a
# block that declares an unrelated routine on every entry no longer retires
# them, while a block that adds a candidate to the family itself still does.
# Every expectation was checked against rakudo. The resolution count is pinned
# by tests/multi_family_generation.rs (a MUTSU_VM_STATS check).

plan 4;

multi f(Int $x) { 'int' }
multi g(Int $x) { 'g-int' }

{
    my @r;
    my $*B = 'b';
    for ^3 -> $i {
        {
            multi f(Str $x) { 'str' }
            @r.push: f('a');
            @r.push: f(1);
            @r.push: g($i);
        }
        @r.push: (try f($*B)) // 'none';
        @r.push: f($i);
    }
    is @r.join(','),
        (|<str int g-int none int> xx 3).join(','),
        "a block's own candidate of the family joins it inside and leaves with the block";
}

{
    my @s;
    for ^3 -> $i {
        { multi h(Int $z) { $z * 10 }; @s.push: h($i) + g($i).chars }
        @s.push: f($i);
    }
    is @s.join(','), '5,int,15,int,25,int',
        'an unrelated family declared per block entry leaves the outer family intact';
}

{
    my $s = 0;
    for ^20 -> $i {
        { multi k(Int $z) { $z }; $s += k(1) }
        $s += f($i) eq 'int' ?? 1 !! 100;
    }
    is $s, 40, 'the cached winner keeps answering across the other family moving';
}

{
    # A candidate added by EVAL moves the family: the next call re-dispatches.
    multi m(Int $x) { 'int' }
    my @r = m(1);
    EVAL q[multi m(Int $x where * > 5) { 'big' }; @r.push: m(9); @r.push: m(1)];
    is @r.join(','), 'int,big,int', 'a new candidate of the family is seen by the cached call site';
}
