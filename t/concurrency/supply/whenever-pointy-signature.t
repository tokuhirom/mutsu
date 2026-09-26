use Test;

# A `whenever`'s pointy block takes any signature an ordinary pointy block
# does. It used to be parsed by a single-parameter-only rule, so a
# sub-signature (Temp::Path's `whenever $GOODS -> ($_, $path?) { ... }`)
# fragmented the statement and failed to compile (#9492).

plan 9;

{
    my $c = Channel.new;
    $c.send(('add', 'x'));
    $c.send(('nuke',));
    $c.close;
    my @seen;
    react whenever $c -> ($cmd, $arg?) {
        @seen.push: "$cmd:" ~ ($arg // '-');
    }
    is-deeply @seen, ['add:x', 'nuke:-'], 'sub-signature with an optional param';
}

{
    my @seen;
    react whenever Supply.from-list(('a', 1), ('b', 2)) -> ($_, $n) {
        when 'a' { @seen.push: "A$n" }
        default  { @seen.push: "other$n" }
    }
    is-deeply @seen, ['A1', 'other2'], 'sub-signature binding $_ drives `when`';
}

{
    my @sums;
    react whenever Supply.from-list([1, 2], [3, 4]) -> [$a, $b] {
        @sums.push: $a + $b;
    }
    is-deeply @sums, [3, 7], 'array sub-signature destructures each emitted value';
}

{
    my @seen;
    react whenever Supply.from-list(5, 9) -> $x where * > 2 {
        @seen.push: $x;
        LAST { @seen.push: 'last' }
    }
    is-deeply @seen, [5, 9, 'last'], 'where-constrained pointy param with a LAST phaser';
}

{
    my @seen;
    react whenever Supply.from-list(<p q>) -> \row { @seen.push: row }
    is-deeply @seen, ['p', 'q'], 'sigilless pointy param still binds';
}

{
    my @seen;
    react whenever Supply.from-list(1, 2) -> Int $x { @seen.push: $x * 10 }
    is-deeply @seen, [10, 20], 'typed pointy param still binds';
}

{
    my @seen;
    react whenever Supply.from-list(3, 4) { @seen.push: $^v }
    is-deeply @seen, [3, 4], 'placeholder parameter still binds';
}

{
    my @seen;
    my $c = Channel.new;
    # The Temp::Path shape: a `start react whenever` command loop that the
    # program shuts down by sending a final command and awaiting `.closed`.
    start react whenever $c -> ($_, $path?) {
        when 'add'  { @seen.push: $path }
        when 'stop' { $c.close }
    }
    $c.send(('add', 'f'));
    $c.send(('stop',));
    await $c.closed;
    is-deeply @seen, ['f'], '`start react whenever` command loop with a sub-signature';
}

{
    my $s = supply {
        whenever Supply.from-list((1, 2), (3, 4)) -> ($a, $b) { emit $a * $b }
    }
    is-deeply $s.list, (2, 12), 'sub-signature whenever inside a supply block';
}
