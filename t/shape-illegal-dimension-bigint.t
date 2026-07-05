use Test;

# A shaped-array dimension that overflows i64 (parsed as a BigInt) must throw
# X::IllegalDimensionInShape, like small illegal dims. `-9223372036854775808`
# parses as a BigInt (the unsigned literal overflows i64 before negation), and a
# huge positive dimension is unallocatable.

plan 8;

throws-like 'my @a[0]', X::IllegalDimensionInShape, 'zero dimension';
throws-like 'my @a[-5]', X::IllegalDimensionInShape, 'small negative dimension';
throws-like 'my @a[-9223372036854775808,-2]', X::IllegalDimensionInShape,
    'i64::MIN (BigInt) dimension, multi-dim';
throws-like 'my @a[99999999999999999999]', X::IllegalDimensionInShape,
    'huge positive BigInt dimension';
throws-like 'my @a[-99999999999999999999]', X::IllegalDimensionInShape,
    'huge negative BigInt dimension';

# The message keeps the exact (BigInt) value
my $msg;
{ my @a[99999999999999999999]; CATCH { default { $msg = .message } } }
is $msg, 'Illegal dimension in shape: 99999999999999999999. All dimensions must be integers bigger than 0',
    'exact BigInt value in the message';

# Valid dimensions still work
lives-ok { my @a[3]; @a[0] = 1 }, 'valid dimension lives';
{
    my @a[2; 3];
    is @a.shape.gist, '(2 3)', 'valid multi-dim shape';
}
