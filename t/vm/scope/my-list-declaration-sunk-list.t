use Test;

# A sunk `my ($a, $b) = ...` / `($a, $b);` must not box the variables into
# shared cells (perf: it pushed every later plain store off the fast lane),
# while the observable semantics stay those of Raku.
plan 6;

sub inv(Int $n, Int $m) {
    my ($i, $h, $v, $d) = $m, $n, 0, 1;
    repeat {
        my $t = $i div $h;
        my $x = $h;
        $h = $i - $t * $x;
        $i = $x;
        $x = $d;
        $d = $v - $t * $x;
        $v = $x;
    } while $h > 0;
    $v mod $m;
}
is inv(3, 7), 5, 'extended Euclid with a list declaration';
is (inv(3, 7) * 3) % 7, 1, 'result is a modular inverse';

{
    my ($a, $b) = 1, 2;
    $a = 10; $b = 20;
    is "$a $b", "10 20", 'plain stores after a list declaration';
}

{
    my $a = 1; my $b = 2;
    my $l = ($a, $b);
    $a = 99;
    is $l.join(","), "99,2", 'a List held in a variable still aliases its scalars';
}

{
    my ($a, $b) = 1, 2;
    my $r = (my ($c, $d) = 3, 4);
    is $r.join(","), "3,4", 'the declaration used as a value still yields the list';
    $c = 5;
    is $c, 5, 'and its variables remain assignable';
}
