use Test;

plan 2;

sub countup_nomod_unnamed {
    my $num = @_.shift;
    return $num if $num <= 0;
    return countup_nomod_unnamed($num - 1), $num;
}

sub countdown_nomod_unnamed {
    my $num = @_.shift;
    return $num if $num <= 0;
    return $num, countdown_nomod_unnamed($num - 1);
}

is countup_nomod_unnamed(5).flat.join, "012345", "return accepts comma-list for recursive count-up";
is countdown_nomod_unnamed(5).flat.join, "543210", "return accepts comma-list for recursive count-down";
