use Test;

# #12287: `does` on a method object composes into the declaration, so a
# later lookup of the same method sees the role.
plan 6;

role R { method hi { 'hi' } }
role S { method yo { 'yo' } }
class K { method m { 1 } }

my $m = K.^find_method('m');
$m does R;
ok K.^find_method('m') ~~ R, 'a later .^find_method sees the role';
is K.^lookup('m').hi, 'hi', 'a later .^lookup answers the role method';
is $m.hi, 'hi', 'the object itself has it';
$m does S;
is K.^find_method('m').yo, 'yo', 'a second does accumulates';
is K.^find_method('m').hi, 'hi', 'the first role is kept';
is K.new.m, 1, 'the method still runs';
