use Test;

# `%%` and `%` by a zero divisor must produce a soft Failure (like `div`
# already does), not throw eagerly at the operator. See
# https://github.com/tokuhirom/mutsu/issues/7752
plan 13;

# The four repro lines from the issue.
{
    my $f = 6 %% 0;
    is $f.^name, 'Failure', '6 %% 0 is a Failure, not an eager throw';
}
{
    my $g = 6 % 0;
    is $g.^name, 'Failure', '6 % 0 is a Failure, not an eager throw';
}
is (so (6 %% 0)), False, 'so (6 %% 0) is False';
is-deeply (^10).grep({ 10 %% $_ }).List, (1, 2, 5).List,
    'grep with 10 %% * skips the zero divisor instead of dying';

# Both Int and Num operands behave the same.
{
    my $n = 6e0 %% 0;
    is $n.^name, 'Failure', '6e0 %% 0 (Num) is also a Failure';
}
{
    my $m = 6e0 % 0;
    is $m.^name, 'Failure', '6e0 % 0 (Num) is also a Failure';
}

# The Failure still throws when sunk or when a value is demanded.
{
    my $died = False;
    { (6 %% 0).sink; CATCH { default { $died = True } } }
    ok $died, 'a sunk %% 0 Failure throws';
}
{
    my $died = False;
    { (6 % 0).sink; CATCH { default { $died = True } } }
    ok $died, 'a sunk % 0 Failure throws';
}
{
    my $died = False;
    { say 6 %% 0; CATCH { default { $died = True } } }
    ok $died, 'say 6 %% 0 demands the value and dies';
}

# `!%%` negates through the Failure (boolifying it marks it handled), so it
# never dies on a zero divisor.
{
    my $p = 6 !%% 0;
    is $p.^name, 'Bool', '6 !%% 0 is a plain Bool, not a Failure';
    is $p, True, '6 !%% 0 is True';
}

# Controls that were already correct stay correct.
{
    my $h = 6 div 0;
    is $h.^name, 'Failure', '6 div 0 is still a Failure (unaffected control)';
}
is-deeply (^10).grep({ $_ %% 2 }).List, (0, 2, 4, 6, 8).List,
    'grep with * %% 2 (no zero divisor) is unaffected';

done-testing;
