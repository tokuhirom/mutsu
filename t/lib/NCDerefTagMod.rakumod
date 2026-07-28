use NativeCall;
unit module NCDerefTagMod;

# `NativeHelpers::Blob` reaches MoarVM's array body through
# `nativecast(Pointer[type], OBJECT_BODY(any)).deref`, where the target class is
# a lexical CStruct inside a module and comes out of a hash at runtime. The
# handle that comes back must carry the class's *registered* name, or its
# hand-written methods are unreachable (only the generated accessors resolve,
# via their short-name fallback). Uses libc only (CI-safe).

my class LexBody is repr('CStruct') {
    has uint64 $.a;
    has uint64 $.b;
    method sum(::?CLASS:D:) { $!a + $!b }
}

my %known = (body => LexBody);

sub calloc(size_t, size_t --> Pointer) is native { * }

#| The NativeHelpers::Blob shape: target class from a hash, cast via Pointer[T].
sub deref-body(--> Mu) is export {
    my \type = %known<body>;
    nativecast(Pointer[type], calloc(1, 32)).deref;
}

#| Same, with the type written literally.
sub deref-body-literal(--> Mu) is export {
    nativecast(Pointer[LexBody], calloc(1, 32)).deref;
}

#| The already-working `nativecast(T, …)` spelling, kept as the control.
sub cast-body(--> Mu) is export {
    nativecast(LexBody, calloc(1, 32));
}
