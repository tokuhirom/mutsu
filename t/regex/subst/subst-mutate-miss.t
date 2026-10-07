use Test;

plan 6;

my $s = "abc";
ok $s.subst-mutate("zzz", "y") === Nil, 'literal miss answers Nil';
ok $s.subst-mutate(/zzz/, "y") === Nil, 'regex miss answers Nil';
is-deeply $s.subst-mutate(/zzz/, "y", :g), (), ':g miss answers an empty list';
is-deeply $s.subst-mutate(/zzz/, "y", :x(2)), (), ':x miss answers an empty list';
is $s, "abc", 'a miss leaves the string unchanged';
isa-ok $s.subst-mutate(/b/, "y"), Match, 'a hit still answers a Match';
