use v6;
use lib 't/lib/ResSplit/lib';
use Test;

plan 6;

# `%?RESOURCES<key>.copy($dest)` copies the resource's file, with the
# destination as a Str or as an IO::Path (#16191). Only the copy is made; the
# resource itself is never moved or changed.
use ResSplit;

my $r = strs();
my $expected = $r.slurp;
my $dir = $*TMPDIR.add("res-copy-$*PID-{(1..1_000_000).pick}");
$dir.mkdir;
LEAVE { try { .unlink for $dir.dir; $dir.rmdir } }

my $to-path = $dir.add('a.bin');
ok $r.copy($to-path), 'copy to an IO::Path destination answers True';
is $to-path.slurp, $expected, 'the copy has the resource content';

my $to-str = $dir.add('b.bin').Str;
ok $r.copy($to-str), 'copy to a Str destination answers True';
is $to-str.IO.slurp, $expected, 'the Str-destination copy has the content';

ok $r.copy($dir.add('c.bin'), :createonly), ':createonly is accepted';
is $r.slurp, $expected, 'the resource itself is unchanged';
