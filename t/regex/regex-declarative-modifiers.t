use Test;

plan 6;

{
    my $a = 1;
    my regex ma { $a $a };
    my regex ta { :temp $a = 5; <&ma> };
    ok '55' ~~ m/^ <ta> $/, ':temp applies while matching nested named regex';
    is $a, 1, ':temp restores variable after match';
}

{
    my $a = 1;
    my regex lma { $a $a };
    my regex la { :let $a = 5; <&lma> };
    nok '23' ~~ m/^ <la> $/, ':let does not force an unsuccessful match';
    is $a, 1, ':let restores value after unsuccessful match';
    ok '55' ~~ m/^ <la> $/, ':let updates value for successful match';
    is $a, 5, ':let keeps updated value after successful match';
}
