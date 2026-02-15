use Test;
plan 3;

my $x = 0;
dies-ok { die "x" }, 'dies-ok via pair-encoded named args', :todo(False);
ok True, 'dies-ok ran and returned';

ok 1, 'ok with named todo via pair-encoded args', :todo(False);
