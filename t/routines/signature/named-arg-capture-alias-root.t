use Test;
plan 3;

# `key => $var` retains $var's container (#16002, #16121); capturing it must
# not change what the callee can write or what a constructor stores.
sub w(:$p is raw) { $p = 5 }
sub via(|c) { w |c }

my $plain;
via :p($plain);
is $plain, 5, 'a plain variable is written through |c';

class K { has $.x }
my $v = 3;
my $k = K.new(x => $v);
$v = 4;
is $k.x, 3, 'a constructed attribute holds the value';

my $topic-seen;
for 1, 2 -> $t { $topic-seen = K.new(x => $t).x }
is $topic-seen, 2, 'a loop parameter as a named argument';
