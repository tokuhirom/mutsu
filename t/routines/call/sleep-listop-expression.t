use Test;

plan 1;

my $started = now;
sleep 0 + 0.2;
my $elapsed = now - $started;

ok $elapsed >= 0.15, "sleep listop parses full infix expression argument";
