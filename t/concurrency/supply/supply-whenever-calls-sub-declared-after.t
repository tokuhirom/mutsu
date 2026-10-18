use Test;

# A `whenever` body calling a sub the `supply` block declares after it
# reaches this activation's sub, which shares the block's lexicals (#13613).
# The supply body is recompiled from its AST when the supply is tapped; the
# recompile records the block's routines as bindings for its closures, as the
# definition-site compile does.

plan 5;

{
    my @seen;
    my $s = supply {
        my @u = 1, 2;
        whenever Supply.from-list(1) { acc() }
        sub acc() { @u.push(3); @seen.push(@u.join(',')) }
    }
    $s.tap;
    is @seen.join('|'), '1,2,3', 'a synchronous whenever pushes into the block lexical';
}

{
    my @seen;
    my $s = supply {
        my @u = 1, 2;
        whenever Promise.in(0.01) { acc(); done }
        sub acc() { @u.push(3); @seen.push(@u.join(',')) }
    }
    react { whenever $s { } }
    is @seen.join('|'), '1,2,3', 'a whenever that fires after the body finished';
}

{
    my @seen;
    my $s = supply {
        my @u = 'sup';
        whenever Supply.from-list(1) { acc() }
        sub acc { @u.push('x'); @seen.push(@u.join(',')) }
    }
    $s.tap;
    $s.tap;
    is @seen.join('|'), 'sup,x|sup,x', 'each tap runs its own activation';
}

{
    my @got;
    my $s = supply {
        my @q = 1, 2, 3;
        whenever Supply.from-list(1) { drain() }
        sub drain { while @q.shift -> $j { emit $j } }
    }
    $s.tap({ @got.push($_) });
    is @got.join(','), '1,2,3', 'a sub declared after the whenever drains the buffer';
}

{
    sub mk { my @u = 'mk'; my $r; Supply.from-list(1).tap({ $r = acc() }); sub acc { @u.join } ; $r }
    my @seen;
    my $s = supply {
        my @u = 'sup';
        whenever Supply.from-list(1) { @seen.push(acc()) }
        sub acc { @u.join }
    }
    mk();
    $s.tap;
    is @seen.join, 'sup', "another routine's same-named sub is not the one called";
}
