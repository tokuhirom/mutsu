use v6;
use Test;

# A lazily pulled `gather` suspended inside a `for` nested in a `while` / `loop`
# body used to resume by replaying the body from its first statement, so
# `my $d = @q.shift` ran again on every resume ("Cannot shift from an empty
# Array", or elements silently skipped). The resumed iteration now re-enters at
# the nested loop. Found with App::Ariza t/01-resources. Identical under raku.

plan 6;

my @a = (gather {
    my @q = 1, 2, 3;
    while @q {
        my $d = @q.shift;
        for 1..2 -> $e { take $d * 10 + $e }
    }
}).map({ $_ * 2 });
is-deeply @a, [22, 24, 42, 44, 62, 64], 'while + shift + nested for, mapped lazily';

my @b = (gather {
    my @q = 1, 2, 3;
    loop (my $i = 0; $i < 3; $i++) {
        my $d = @q.shift;
        for 1..2 -> $e { take $d * 10 + $e }
    }
}).map({ $_ * 2 });
is-deeply @b, [22, 24, 42, 44, 62, 64], 'C-style loop';

my @d = (gather {
    my @q = 1, 2, 3;
    while @q {
        my $d = @q.shift;
        if $d != 2 { for 1..2 -> $e { take $d * 10 + $e } } else { take 99 }
    }
}).map({ $_ * 2 });
is-deeply @d, [22, 24, 198, 62, 64], 'for inside an if inside the while';

my @e = (gather {
    my @q = 1, 2, 3;
    until !@q {
        my $d = @q.shift;
        for <a b> -> $e { take "$d$e" }
    }
}).map({ .uc });
is-deeply @e, ["1A", "1B", "2A", "2B", "3A", "3B"], 'until';

my $n = 0;
my @f = (gather {
    my @q = 1, 2;
    while @q {
        my $d = @q.shift;
        $n++;
        for 1..2 -> $e { take $d * 10 + $e }
    }
}).map({ $_ * 2 });
is-deeply @f, [22, 24, 42, 44], 'side-effecting statement before the for';
is $n, 2, 'it ran once per iteration, not once per resume';
