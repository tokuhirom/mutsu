use v6;
use Test;

# From MIDI::Make: `Buf.write-uint32(0, $n, BigEndian).splice(1)` calls `splice`
# on a Buf no variable holds, which died with "No such method 'splice'".
plan 6;

sub four() { Buf.write-uint32(0, 500000, BigEndian) }

is four().splice(1), Buf.new(7, 0xA1, 0x20), 'splice(offset) of a temporary answers the removed bytes';
is four().splice(1, 2), Buf.new(7, 0xA1), 'splice(offset, count)';
is four().splice(0, 0), Buf.new, 'a zero count removes nothing';

my $kept = Buf.new(1, 2, 3, 4);
is $kept.splice(1, 1), Buf.new(2), 'a named Buf still splices';
is $kept, Buf.new(1, 3, 4), 'and loses the removed bytes';

class Holder { has Buf $.b = Buf.new(1, 2, 3, 4) }
my $h = Holder.new;
$h.b.splice(0, 2);
is $h.b, Buf.new(3, 4), 'a Buf held by an attribute loses the removed bytes';
