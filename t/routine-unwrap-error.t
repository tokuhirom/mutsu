use Test;

# &routine.unwrap with something that is not a valid wrap handle throws
# X::Routine::Unwrap.

plan 3;

throws-like 'sub f() { }; &f.unwrap("foo")', X::Routine::Unwrap,
    'unwrap with a non-handle argument';

throws-like 'sub g() { }; &g.unwrap', X::Routine::Unwrap,
    'unwrap of a never-wrapped routine';

# A valid wrap/unwrap round-trip still works.
lives-ok {
    my $h = &?ROUTINE.WHO ?? Nil !! Nil;  # no-op to keep block non-empty
    sub h() { 1 }
    my $handle = &h.wrap(sub { callsame() + 10 });
    die "wrap broken" unless h() == 11;
    &h.unwrap($handle);
    die "unwrap broken" unless h() == 1;
}, 'wrap then unwrap with a real handle works';
