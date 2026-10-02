use Test;

# A pointy `given`/`with` whose topic is not a variable must not write its
# parameter back into a variable an earlier expression-context assignment
# touched. `while COND -> $x` lowers to `while ($x = COND)`, so the
# `given $/.made -> $message { ... }` in Stomp::MessageStream's parse loop
# overwrote the loop's `$/` with the made value.

plan 7;

sub q { 'ab' }

{
    my $n = 0;
    my $seen;
    while q() -> $x {
        given 5 -> $m { }
        $seen = $x;
        last if ++$n > 0;
    }
    is $seen, 'ab', 'a pointy given leaves the while loop variable alone';
}

{
    my $y;
    if ($y = q()) {
        given 5 -> $m { }
    }
    is $y, 'ab', 'a pointy given leaves an assigned if condition alone';
}

{
    my $y;
    my $z = ($y = q());
    given 5 -> $m { }
    is $y, 'ab', 'a pointy given leaves an earlier assignment expression alone';
}

{
    my $y;
    my $z = ($y = q());
    with 7 -> $m { }
    is $y, 'ab', 'a pointy with leaves an earlier assignment expression alone';
}

{
    grammar G { token TOP { a } }
    class A { method TOP($/) { make 'MADE' } }
    my $buffer = 'aab';
    my @made;
    while G.subparse($buffer, actions => A) -> $/ {
        given $/.made -> $m { @made.push($m) }
        $buffer .= substr($/.chars);
    }
    is-deeply @made, ['MADE', 'MADE'], 'a subparse loop with a pointy given sees each match';
}

{
    my $x = 1;
    given $x -> $m is rw { $m = 7 }
    is $x, 7, 'a variable topic still writes an is rw parameter back';
}

{
    my @a = 1, 2;
    given @a -> @p { @p.push(3) }
    is-deeply @a, [1, 2, 3], 'an array topic still sees its pointy parameter mutated';
}
