use v6;
use lib 't/lib';
use Test;
use HookOperatorFamily;
use HookOperatorFamily::Caller;

plan 2;

# `++` on an Int variable whose operator family a `sub EXPORT` hook imported
# is the core increment of the variable, also when the routine doing it is
# called back from another unit (Red's CHECK-built getters, t/091).
my $result = EVAL q:to/CODE/;
    my $out;
    {
        my $count = 0;
        sub bump() { ++$count; $count++; 23 }
        bump();
        call-it({ bump() });
        $out = $count;
    }
    $out
    CODE
is $result, 4, 'a called-back routine increments its captured variable';
is Thing++, "thing++", 'the family still answers for its own type';
