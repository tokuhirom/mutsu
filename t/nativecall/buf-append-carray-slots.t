use v6;
use Test;
use NativeCall;

# Archive::Libarchive::Raw (ecosystem): appending CArray[uint8] slots (which
# are Proxies over C memory) to a Buf appends their values.
plan 6;

sub malloc(size_t --> Pointer) is native {*}
my $w = nativecast(CArray[uint8], malloc(8));
$w[0] = 80; $w[1] = 75; $w[2] = 3;

my $a = Buf.new; $a.append: $w[^3];
is $a.list, (80, 75, 3), 'append a CArray slice';
my $b = Buf.new; $b.append: ($w[0], $w[1]);
is $b.list, (80, 75), 'append a list of slots';
my $c = Buf.new; $c.append: $w[0, 1, 2];
is $c.list, (80, 75, 3), 'append a slot list slice';
my $d = Buf.new; $d.push: $w[2];
is $d.list, (3,), 'push a slot';
my $e = Buf.new(1); $e.unshift($w[0]); $e.unshift($w[1]);
is $e.list, (75, 80, 1), 'unshift slots';
my $f = Buf.new; $f.append: (80, 75, 3).Seq;
is $f.list, (80, 75, 3), 'a plain Seq still appends';
