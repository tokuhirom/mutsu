use Test;

# A multi-parameter `for` loop declares its parameters as fresh lexicals of
# the loop block. Inside a routine, a parameter whose name matches a `:=`-bound
# variable of the enclosing scope must not write through that binding (#9689).

plan 6;

{
    my @a = 10, 20;
    my $child := @a[0];
    sub g(@l) { for @l.kv -> $i, $child { } }
    g([1, 2]);
    is-deeply @a, [10, 20], 'a sub loop parameter does not write through an outer := binding';
}

{
    class N {
        has @.children;
        method names() {
            my @n;
            for @.children.kv -> $i, $child { @n.push($child.^name) }
            @n
        }
    }
    my $n = N.new(children => (N.new,));
    my $child := $n.children[0];
    is-deeply $n.names, ['N'], 'a method loop parameter sees its own value, not the outer binding';
}

{
    my $x = 5;
    sub h() { for (1, 2).kv -> $i, $x { } }
    h();
    is $x, 5, 'an outer scalar survives a same-named sub loop parameter';
}

{
    my @out;
    for 1, 2, 3, 4 -> $p, $q {
        for 7, 8 -> $p, $q { @out.push("$p-$q") }
        @out.push("outer $p-$q");
    }
    is-deeply @out, ['7-8', 'outer 1-2', '7-8', 'outer 3-4'],
        'a nested same-named multi-param loop shadows the outer one';
}

{
    my @rounds;
    for 1..2 -> $i {
        for (10, 20, 30, 40) -> $a, $i { }
        @rounds.push($i);
    }
    is-deeply @rounds, [1, 2], 'an outer single-param loop variable survives an inner multi-param shadow';
}

{
    my @c;
    for 10, 20, 30, 40 -> $x, $y { @c.push(-> { $x + $y }) }
    is-deeply @c.map({ $_() }).List, (30, 70), 'closures capture each iteration\'s own parameters';
}
