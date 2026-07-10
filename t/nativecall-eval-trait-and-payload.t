# NativeCall + exception-payload fixes surfaced by zef's
# `!native-library-is-installed` probe (distribution-depends-parsing test 35,
# native `:any` dep case):
#   1. A `sub ... is native('<missing>')` declared inside EVAL'd source must be
#      accepted (the interpreter registration path previously rejected `native`
#      as an unknown trait) and, when called, must die trying to load the
#      library.
#   2. The dlopen-failure message must match Rakudo's shape
#      ("Cannot locate native library '<lib>': ...") so code that inspects it
#      (`.payload.starts-with("Cannot locate native library")`) works.
#   3. `X::AdHoc.payload` must return the die payload, falling back to the
#      message string for an X::AdHoc synthesized from an internal error.

use Test;
plan 7;

# --- .payload on a plain `die "string"` ---
{
    my $exc = do { try { die "boom-payload" }; $! };
    is $exc.^name, 'X::AdHoc', 'die "str" produces X::AdHoc';
    is $exc.payload, 'boom-payload', '.payload returns the die string';
}

# --- .payload on `die $typed-object` returns the object ---
{
    my $obj = [1, 2, 3];
    my $exc = do { try { die $obj }; $! };
    ok $exc.payload ~~ Positional, '.payload returns the thrown object';
}

# --- is native for a missing library: message shape + .payload fallback ---
{
    use NativeCall;
    sub mutsu_missing_native_fn is native('mutsu-no-such-lib-xyz') { * }
    my $exc = do { try { mutsu_missing_native_fn() }; $! };
    ok $exc.defined, 'calling a missing native sub dies';
    ok $exc.message.starts-with('Cannot locate native library'),
        'dlopen failure message matches Rakudo shape';
    ok $exc.payload.starts-with('Cannot locate native library'),
        '.payload falls back to the message for an internal X::AdHoc';
}

# --- is native declared inside EVAL is accepted (not "unknown trait") ---
{
    use MONKEY-SEE-NO-EVAL;
    my $exc = do {
        try {
            EVAL q[use NativeCall; sub eval_native is native('mutsu-no-such-lib-xyz') { !!! }; eval_native();];
        };
        $!;
    };
    ok $exc.defined && $exc.payload.starts-with('Cannot locate native library'),
        'is native inside EVAL registers and dies locating the library';
}
