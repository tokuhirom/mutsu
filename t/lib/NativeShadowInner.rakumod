use NativeCall;
unit module NativeShadowInner;

# Each of these mimics `Compress::Zlib::Raw`'s `sub compress(Blob,
# CArray[long], Blob, ulong)`: a 3-arg native routine, deliberately
# arity-mismatched against the 1-arg Raku-level wrapper of the same bare name
# declared in NativeShadowOuter.rakumod. The library name is intentionally
# bogus -- these native descriptors must never actually be dispatched (that is
# the whole point of the test), so nothing here needs to link successfully.

our sub shadow-our(int32, int32, int32) returns int32
    is native('mutsu-test-nonexistent-lib') is export { * }
our sub shadow-my(int32, int32, int32) returns int32
    is native('mutsu-test-nonexistent-lib') is export { * }
our sub shadow-multi(int32, int32, int32) returns int32
    is native('mutsu-test-nonexistent-lib') is export { * }
our sub shadow-before(int32, int32, int32) returns int32
    is native('mutsu-test-nonexistent-lib') is export { * }
our sub shadow-noexport(int32, int32, int32) returns int32
    is native('mutsu-test-nonexistent-lib') is export { * }
