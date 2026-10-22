use Test;

plan 9;

# The source of a scalar `:=` that is parenthesized or block-valued denotes
# the container of its tail expression, as a bare element does (#14618).
{
    my %h = d => 1;
    my $d := (%h<d>);
    $d = 2;
    is-deeply %h, {d => 2}, 'a parenthesized element';
}
{
    my %h = d => 1;
    my $d := do { %h<d> };
    $d = 3;
    is-deeply %h, {d => 3}, 'a do block ending in an element';
}
{
    my %h = d => 1;
    my $d := do if True { %h<d> } else { 0 };
    $d = 4;
    is-deeply %h, {d => 4}, 'a do if block';
}
{
    my %h = d => 1;
    my $d := do with 1 { %h<d> } else { 0 };
    $d = 5;
    is-deeply %h, {d => 5}, 'a do with block';
}
{
    my %h = d => 1;
    my $n = (s => %h);
    my $d := do { $n.value<d> };
    $d = 6;
    is-deeply %h, {d => 6}, 'an element reached through a method call';
}
{
    # The shape of PDF::IO::Serializer.freeze.
    my $stream;
    my $dict;
    my $node;
    my $node-value := do with $stream {
        my $encoded = .Str;
        $node = :stream{ :$dict, :$encoded };
        $node.value<dict>;
    }
    else {
        $node = :$dict;
        $node.value;
    }
    $node-value = { a => 1 };
    is-deeply $node, (:dict({a => 1})), 'the else branch: a Pair value';

    my $stream2 = "abc";
    my Hash $dict2;
    my $node2;
    my $node-value2 := do with $stream2 {
        my $encoded = .Str;
        $node2 = :stream{ :dict($dict2), :$encoded };
        $node2.value<dict>;
    }
    else {
        $node2 = :dict($dict2);
        $node2.value;
    }
    $node-value2 = { b => 2 };
    is-deeply $node2.value<dict>, {b => 2}, 'the with branch: a hash element of a Pair value';
}

# Reads and the other uses of these forms are unchanged.
{
    my %h = d => 1;
    my $d := do { %h<d> };
    is $d, 1, 'the bound container still reads its value';
    my $e := do { 42 };
    is $e, 42, 'a block whose tail is a plain value binds that value';
}
