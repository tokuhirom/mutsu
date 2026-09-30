use Test;

plan 6;

# A multi-parameter pointy `for` binds named parameters, not the topic, so `$_`
# stays the enclosing topic (#9978).
{
    my @seen;
    $_ = "U";
    for <x y> -> $a, $b { @seen.push($_) }
    is @seen.join(","), "U", "for -> \$a, \$b leaves \$_ alone";
}

{
    my %h = a => "b";
    my @seen;
    given "T" { for %h.kv -> $k, $v { @seen.push($_) } }
    is @seen.join(","), "T", "for %h.kv -> \$k, \$v inside given keeps the given topic";
}

{
    my @out;
    for <p q r s> -> $_, $n { @out.push("$_$n") }
    is @out.join(","), "pq,rs", "-> \$_, \$n still binds \$_ to the first element";
}

{
    my @out;
    for 1..4 -> $a, $b { @out.push("$a$b") }
    is @out.join(","), "12,34", "int-range multi-param loop batches";
}

{
    my @out;
    for (1, 2, 3) -> $a, $b = 9 { @out.push("$a$b") }
    is @out.join(","), "12,39", "defaulted second param sees the short final chunk";
}

{
    my $node = { a => 1 };
    my %rename = a => "b";
    given $node {
        for %rename.kv -> $old, $new {
            if $_{$old}:exists { $_{$new} = $_{$old}; $_{$old}:delete; }
        }
    }
    is $node.keys.join(","), "b", "indexing \$_ inside the loop reaches the given topic";
}
