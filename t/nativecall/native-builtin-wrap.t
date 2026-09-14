use Test;

plan 3;

my $now = now;
my $now-handle = &term:<now>.wrap: { $now };
is now, $now, 'wrapping the native now term affects named term calls';
&term:<now>.unwrap: $now-handle;

my $time-handle = &term:<time>.wrap: { 123 };
is time, 123, 'wrapping the native time term affects named term calls';
&term:<time>.unwrap: $time-handle;

my $slept = False;
my $sleep-handle = &sleep.wrap: { $slept = True; Nil };
sleep 1;
ok $slept, 'wrapping the native sleep routine affects named calls';
&sleep.unwrap: $sleep-handle;
