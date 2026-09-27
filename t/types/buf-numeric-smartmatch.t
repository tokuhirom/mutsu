use Test;

# A Blob/Buf is Positional, and smartmatching one against a number compares
# its element count, like an Array does (`[1,2,3] ~~ 3`). The EC dist's
# ed25519 dispatches its constructors on exactly that: `blob8 $seed where 32`.
# Blob.ACCEPTS(Blob) compares the contents.

plan 14;

my $b = blob8.new(1 xx 32);
ok $b ~~ 32, 'blob8 ~~ its element count';
nok $b ~~ 31, 'blob8 !~~ another number';
ok buf8.new(1, 2) ~~ 2.0, 'buf8 ~~ a Num element count';
ok buf8.new(1, 2) ~~ 2/1, 'buf8 ~~ a Rat element count';
ok 32.ACCEPTS($b), 'Int.ACCEPTS(Blob) numifies the Blob';
ok 3.ACCEPTS([1, 2, 3]), 'Int.ACCEPTS(Array) numifies the Array';

sub by-size(blob8 $s where 32) { 'seed' }
is by-size($b), 'seed', 'a `where N` constraint accepts a Blob of N elements';

class Key {
    multi method new(blob8 $seed where 32)      { 'seed' }
    multi method new(blob8 $seed-hash where 64) { 'seed-hash' }
}
is Key.new(blob8.new(0 xx 32)), 'seed', 'multi dispatch picks the 32-element candidate';
is Key.new(blob8.new(0 xx 64)), 'seed-hash', 'multi dispatch picks the 64-element candidate';

ok blob8.new(1, 2).ACCEPTS(blob8.new(1, 2)), 'Blob.ACCEPTS(equal Blob)';
nok blob8.new(1, 2).ACCEPTS(blob8.new(1, 3)), 'Blob.ACCEPTS(different Blob)';
ok buf8.new(1, 2).ACCEPTS(blob8.new(1, 2)), 'Buf.ACCEPTS(equal Blob)';
nok blob8.new(1, 2).ACCEPTS(2), 'Blob.ACCEPTS(Int) is not a count comparison';
ok blob8.new(1, 2) ~~ blob8.new(1, 2), 'Blob ~~ Blob';
