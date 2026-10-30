use v6;
use Test;

plan 6;

{
    my $s;
    my &f = -> $t { $s := 7; $s := 17 };
    f(1);
    is $s, 17, 'second literal bind of a captured scalar wins';
}

{
    my $s;
    my &f = -> $t { $s := 7; $s := 17; $s := 27 };
    f(1);
    is $s, 27, 'third literal bind wins';
}

{
    my $s = 1;
    my &f = -> $t { $s := 7; $s := 17 };
    f(1);
    is $s, 17, 'also when the outer scalar was initialised';
}

# IRC::Log's search: the outer $seq is bound inside a typed pointy `if`
# block and rebound after a nested `with` block.
sub seq-of($reverse) {
    my $seq;
    if 'a' -> str $t {
        with 3 -> int $pos is copy {
            $seq := 0 .. $pos;
        }
        $seq := $seq.reverse if $reverse;
    }
    $seq.list.gist
}
is seq-of(False), '(0 1 2 3)', 'forward range kept';
is seq-of(True), '(3 2 1 0)', 'reversed rebind is visible after the block';

{
    my $s;
    my &f = -> $t { $s := 7; $s := 17 };
    f(1);
    my &g = -> $t { $s := 99 };
    g(1);
    is $s, 99, 'a later closure still rebinds';
}
