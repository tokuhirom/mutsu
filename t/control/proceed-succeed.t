use Test;
plan 4;

# proceed: continue to next when clause
my $result = '';
given 5 {
    when 5 {
        $result = $result ~ 'a';
        proceed;
    }
    when 5 {
        $result = $result ~ 'b';
    }
}
is $result, 'ab', 'proceed continues to next when';

# succeed: exit the when body early
my $result2 = '';
given 42 {
    when 42 {
        $result2 = $result2 ~ 'x';
        succeed;
        $result2 = $result2 ~ 'y';
    }
}
is $result2, 'x', 'succeed exits when body early';

# default behavior: when matched stops further matching
my $result3 = '';
given 5 {
    when 5 {
        $result3 = $result3 ~ 'first';
    }
    when 5 {
        $result3 = $result3 ~ 'second';
    }
}
is $result3, 'first', 'default when stops at first match';

# redo in a loop
my $count = 0;
my $redo-count = 0;
for 1..3 -> $i {
    $count = $count + 1;
    if $redo-count == 0 && $i == 2 {
        $redo-count = 1;
        redo;
    }
}
is $count, 4, 'redo re-executes loop body';
