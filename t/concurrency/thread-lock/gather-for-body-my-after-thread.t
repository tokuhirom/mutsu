use Test;

plan 4;

# A lazy gather's `for` body that declares a `my` must read the CURRENT
# iteration's value after a thread has been started (#17033): suspending the
# gather restored the "shadowed" value of the body-local `my` as if the loop
# had finished.
await start { 1 };

sub M {
    my $b = 0;
    gather for 1..* -> $j { my $u = $j * 10; take $b; $b = $u; }
}
is (lazy M())[^4].raku, '(0, 10, 20, 30)', 'my copied to an outer variable';

sub N { gather for 1..* -> $j { my $u = $j * 10; take $j; take $u; } }
is (lazy N())[^6].raku, '(1, 10, 2, 20, 3, 30)', 'takes of the loop var and the my';

sub R { my $b = 0; gather for 1..4 -> $j { my $u = $j * 10; take $b; $b = $u; } }
is (lazy R())[^4].raku, '(0, 10, 20, 30)', 'a range with a finite bound';

sub L { my $b = 0; gather for (1, 2, 3, 4) -> $j { my $u = $j * 10; take $b; $b = $u; } }
is (lazy L())[^4].raku, '(0, 10, 20, 30)', 'a list source';
