use Test;

plan 2;

my $out = qx`echo mutsu-qx`;
ok $out ~~ Str, 'qx returns a Str';
is $out.chomp, 'mutsu-qx', 'qx captures stdout';
