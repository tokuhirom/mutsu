use Test;

plan 6;

sub defined-keep($p) { ($p //= 1) += 1 }
sub true-keep($p) { ($p ||= 1) += 1 }
sub false-keep($p) { ($p &&= 1) += 1 }

throws-like { defined-keep(3) }, X::AdHoc,
    message => 'Cannot assign to a readonly variable or a value',
    'a kept defined value retains the read-only parameter container';
throws-like { true-keep(3) }, X::AdHoc,
    message => 'Cannot assign to a readonly variable or a value',
    'a kept true value retains the read-only parameter container';
throws-like { false-keep(0) }, X::AdHoc,
    message => 'Cannot assign to a readonly variable or a value',
    'a kept false value retains the read-only parameter container';

sub read-only-keep($p) { $p //= 1 }
is read-only-keep(3), 3, 'the inner short circuit does not store';

my $value := 42;
throws-like { ($value //= 5) += 1 }, X::Assignment::RO,
    'a value binding still returns an immutable value';

my $writable = 52;
($writable //= 42) += 10;
is $writable, 62, 'a writable scalar still returns its container';
