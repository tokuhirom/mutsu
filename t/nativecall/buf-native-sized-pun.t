use Test;
use nqp;

# From path-utils: `nqp::create(buf8.^pun)` makes a Buf[uint8] it can read into.

plan 5;

is buf8.^pun.^name, 'Buf[uint8]', 'buf8.^pun is Buf[uint8]';
is blob8.^pun.^name, 'Blob[uint8]', 'blob8.^pun is Blob[uint8]';
is buf16.^pun.^name, 'Buf[uint16]', 'buf16.^pun is Buf[uint16]';
my $buf := nqp::create(buf8.^pun);
is $buf.^name, 'Buf[uint8]', 'nqp::create of the pun makes a Buf[uint8]';
nqp::push_i($buf, 7);
is nqp::elems($buf), 1, 'and it is a usable buffer';
