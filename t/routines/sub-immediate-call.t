use Test;

plan 4;

# Basic immediate invocation of sub declaration
{
    my $result = do sub foo($n) { $n * 2 }(21);
    is $result, 42, 'sub declaration with immediate call returns result';
}

# Immediate invocation that calls the sub
sub bar($n) { $n + 1 }(10);
is bar(5), 6, 'sub is still callable after immediate invocation';

# callsame outside dispatcher should throw
{
    my $desc = 'callsame outside dispatcher throws when sub is immediately invoked';
    EVAL q{sub rt69314($n) { if $n { callsame; } }(1)};
    flunk $desc;
    CATCH { default { pass $desc } }
}

# CATCH in bare block catches die
{
    my $desc = 'CATCH in bare block catches die';
    die "oops";
    flunk $desc;
    CATCH { default { pass $desc } }
}
