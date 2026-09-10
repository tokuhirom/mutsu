use v6;
use Test;

# `subbuf-rw($from)` with no length spans from `$from` to the end of the
# buffer, so assigning replaces the whole tail (it does not insert at $from).
# (raku-doc/doc/Type/Buf.rakudoc)

plan 6;

my Buf $b .= new(0..5);
$b.subbuf-rw(3) = Buf.new(200);
is $b.raku, 'Buf.new(0,1,2,200)', 'no-length subbuf-rw replaces the tail';

my Buf $c .= new(0..5);
$c.subbuf-rw(1, 2) = Buf.new(9);
is $c.raku, 'Buf.new(0,9,3,4,5)', 'explicit length still removes exactly that many';

my Buf $d .= new(0..5);
$d.subbuf-rw(2, 0) = Buf.new(99);
is $d.raku, 'Buf.new(0,1,99,2,3,4,5)', 'zero length inserts at $from';

my Buf $e .= new(0..5);
$e.subbuf-rw(6) = Buf.new(7, 8);
is $e.raku, 'Buf.new(0,1,2,3,4,5,7,8)', 'subbuf-rw at end appends';

my Buf $f .= new(0..5);
$f.subbuf-rw(0) = Buf.new(42);
is $f.raku, 'Buf.new(42)', 'from 0, no length replaces everything';

my Buf $g .= new(0..5);
$g.subbuf-rw(2) = Buf.new();
is $g.raku, 'Buf.new(0,1)', 'replacing the tail with an empty Buf truncates';
