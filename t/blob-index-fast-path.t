# Pins the in-place Buf/Blob element read in exec_index_op_with_positional
# (the fast path that bypasses AT-POS method dispatch and whole-buffer
# decode): the observable subscript behavior must stay exactly what the
# dispatch_1arg AT-POS arm produced.
use Test;

plan 12;

my $b = blob32.new(1, 2, 3);
is $b[0], 1, 'blob32 first element';
is $b[2], 3, 'blob32 last element';
is $b[5], 0, 'blob32 out-of-range reads as 0';
my $neg = -1;
ok $b[$neg] === Nil, 'blob32 negative index reads as Nil';

my $c = buf8.new(9, 8, 255);
is $c[1], 8, 'buf8 element';
is $c[2], 255, 'buf8 unsigned byte is not sign-extended';
is $c[10], 0, 'buf8 out-of-range reads as 0';

my $u = utf8.new(65, 66);
is $u[1], 66, 'utf8 (Blob[uint8] alias) element';

is blob64.new(18446744073709551615)[0], 18446744073709551615,
    'blob64 element above i64::MAX decodes as the full unsigned value';

my $s = Blob[int8].new(-1, -128);
is $s[0], -1, 'signed element is sign-extended from its width';
is $s[1], -128, 'signed minimum survives the round-trip';

# Reads must see writes that went through the shared storage node.
my $w = buf8.new(1, 2, 3);
$w[1] = 42;
is $w[1], 42, 'read observes an element written through ASSIGN-POS';
