use v6;
use Test;

plan 19;

my $utf8 = "hi".encode;
my $buf = Buf[uint8].new(104, 105);
my $blob = Blob[uint8].new(104, 105);

throws-like { $buf eq "hi" }, X::Buf::AsStr, 'Buf eq Str throws X::Buf::AsStr';
throws-like { "hi" ne $blob }, X::Buf::AsStr, 'Str ne Blob throws X::Buf::AsStr';
throws-like { $buf lt "hi" }, X::Buf::AsStr, 'Buf lt Str throws X::Buf::AsStr';
throws-like { $buf leg $buf }, X::Buf::AsStr, 'Buf leg Buf throws X::Buf::AsStr';
throws-like { $buf coll $buf }, X::Buf::AsStr, 'Buf coll Buf throws X::Buf::AsStr';
throws-like { $buf unicmp $buf }, X::Buf::AsStr, 'Buf unicmp Buf throws X::Buf::AsStr';

dies-ok { $utf8 lt $buf }, 'mixed Blob types die for lt';
dies-ok { $utf8 gt $buf }, 'mixed Blob types die for gt';
dies-ok { $utf8 le $buf }, 'mixed Blob types die for le';
dies-ok { $utf8 ge $buf }, 'mixed Blob types die for ge';
dies-ok { $utf8 cmp $buf }, 'mixed Blob types die for cmp';
dies-ok { $utf8 before $buf }, 'mixed Blob types die for before';
dies-ok { $utf8 after $buf }, 'mixed Blob types die for after';

ok $buf eq $blob, 'eq still compares bytes across Blob types';
ok $buf le Buf[uint8].new(104, 106), 'same-type ordering compares bytes';
is $buf cmp Buf[uint8].new(104, 105), Same, 'same-type cmp compares bytes';
is $utf8 leg "hi".encode, Same, 'utf8 leg utf8 compares decoded strings';
is $utf8 coll "hi".encode, Same, 'utf8 coll utf8 compares decoded strings';

dies-ok { cmp-ok $buf, "eq", "hi" }, 'cmp-ok propagates Blob stringification errors';
