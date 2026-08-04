use v6;
use Test;

plan 5;

# A native-typed array truncates every stored value to its element width, and
# that must not depend on whether the process has ever spawned a thread. The
# VM's push op has a separate `shared_vars_active` branch -- taken from the
# first `start` onwards, for the rest of the process -- which skipped both the
# element type check and the native-width wrap. `my uint32 @W` therefore
# stopped truncating after any `start` anywhere, and `Digest::SHA1`, whose
# message schedule relies on that truncation, silently produced a wrong digest.

sub build-uint32() {
    my uint32 @W = 1, 2;
    @W.push: 0x1_8589_8e01;   # one bit above 32 bits
    @W[2]
}

is build-uint32(), 0x8589_8e01, 'uint32 array push truncates to 32 bits';

await start { 1 + 1 };

is build-uint32(), 0x8589_8e01,
    'and still truncates after a thread has been spawned';

my int8 @b;
@b.push: 200;
is @b[0], -56, 'int8 array push wraps into the signed range after a start';

my uint8 @u;
@u.push: 300, 301;
is @u.join(','), '44,45', 'every value of a multi-value push is wrapped';

# The element TYPE check lives on the same path and must survive too.
my Int @typed;
throws-like { @typed.push: "not an int" }, X::TypeCheck,
    'a typed array still rejects a bad push after a start';
