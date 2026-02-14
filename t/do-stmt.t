use Test;
plan 6;

my $v1 = do if True { 42 };
is $v1, 42, 'do if returns then-branch value';

my $v2 = do if False { 42 };
ok !$v2, 'do if without else returns Empty (falsey)';

my $v3 = do if False { 1 } elsif True { 7 } else { 9 };
is $v3, 7, 'do elsif chain returns matching branch value';

my $v4 = do given 42 {
    when 42 { "hit" }
    default { "miss" }
};
is $v4, "hit", 'do given returns matching when value';

my $v5 = do given 7 {
    when 42 { "no" }
    default { "fallback" }
};
is $v5, Nil, 'do given default branch returns Nil';

my $v6 = do given 5 {
    when 5 { proceed }
    when 5 { "next-branch" }
};
is $v6, "next-branch", 'do given with proceed returns later when value';
