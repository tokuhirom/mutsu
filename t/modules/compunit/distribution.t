use Test;

plan 5;

# $?DISTRIBUTION should be Nil at the top level (no distribution context)
nok $?DISTRIBUTION.defined, '$?DISTRIBUTION is not defined at top level';

# Test with a module that has META6.json
{
    use lib 'roast/packages/CurrentDistributionOne/lib';
    use CurrentDistributionOne;
    my $d = CurrentDistributionOne::distribution();
    ok $d.defined, 'CurrentDistributionOne::distribution() returns defined value';
    ok $d.meta.defined, '.meta returns defined value';
    ok $d.meta{"provides"}.defined, '.meta{"provides"} is defined';
    ok $d.meta{"provides"}{"CurrentDistributionOne"}.defined, '.meta{"provides"}{"CurrentDistributionOne"} is defined';
}
