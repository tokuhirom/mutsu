use Test;
plan 3;

my $v1 = do if True { 42 };
is $v1, 42, 'do if returns then-branch value';

my $v2 = do if False { 42 };
ok !$v2, 'do if without else returns Empty (falsey)';

my $v3 = do if False { 1 } elsif True { 7 } else { 9 };
is $v3, 7, 'do elsif chain returns matching branch value';
