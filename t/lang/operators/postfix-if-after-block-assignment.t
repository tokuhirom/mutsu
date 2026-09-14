use Test;

plan 1;

my $called = 0;
sub routine { $called++; 7 }
my %handles;
%handles<routine> = &routine.wrap: { callsame }
if False {
    $called = 99;
}

is routine(), 7, 'a prefix if after a block-valued assignment is a new statement';
&routine.unwrap: %handles<routine>;
