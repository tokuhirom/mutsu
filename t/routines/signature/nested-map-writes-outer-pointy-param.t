use v6;
use Test;

# A pointy-block `is copy` parameter written by a nested map/grep/first block
# must keep the write (from the envy distribution's CRC32 table builder, whose
# `(0..7).map({ $a +^= ...; $a +>= 1 })` ran inside an outer `.map(-> $a is copy)`).

plan 6;

is-deeply (1, 2).map(-> $a is copy { (0..1).map({ $a += 10 }); $a }).List, (21, 22),
    'nested map writes an outer map pointy `is copy` param';

is-deeply (1, 2).map(-> $a is copy { (0..1).map({ $a +>= 1 }); $a }).List, (0, 0),
    'nested map shifts an outer pointy param';

my @seen;
for 1, 2 -> $a is copy {
    (0..1).map({ $a += 10 });
    @seen.push($a);
}
is-deeply @seen, [21, 22], 'nested map writes a for-loop `is copy` param';

@seen = ();
for 1 -> $a is copy {
    (0..1).grep({ $a += 10 });
    @seen.push($a);
    (0..1).first({ $a += 1; False });
    @seen.push($a);
}
is-deeply @seen, [21, 23], 'nested grep and first write a for-loop param';

my $x = 5;
(1, 2).map({ my $x = 100; (0..1).map({ $x += 1 }); $x });
is $x, 5, 'an unrelated same-named outer lexical is untouched';

sub bump($n is copy) { (0..2).map({ $n *= 2 }); $n }
is bump(1), 8, 'a sub `is copy` param written by a nested map';
