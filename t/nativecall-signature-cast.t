use v6;
use Test;
use NativeCall;

# Two ways a NativeCall binding reaches C that do not go through a plain
# `is native` call site:
#
#  1. Calling a declared native sub through a *code object* (`my &f = &g; f()`).
#     `NativeLibs::Loader.symbol` picks between `dlFindSymbol` and `dlsym` that
#     way; running the `{ * }` stub instead returned the Whatever `*`.
#  2. `nativecast(<Signature>, $ptr)` — attaching a signature to a function
#     pointer looked up at runtime, which is what makes such a symbol callable
#     at all.

plan 7;

sub getpid(--> int32) is native { * }

my $direct = getpid();
ok $direct > 0, 'a direct native call works';

my &via-ref = &getpid;
is via-ref(), $direct, 'calling a native sub through a code object dispatches to C';
is (&getpid)(), $direct, 'calling a native sub through an inline code object works too';

my @through-map = (1,).map({ &getpid })».();
is @through-map[0], $direct, 'a native code object called from a closure dispatches to C';

# Cast a function pointer to a signature. `dlsym`/`dlopen` are in the process
# already (mutsu links libc), so look up a libm symbol through them.
sub dlopen(Str, uint32 --> Pointer) is native { * }
sub dlsym(Pointer, Str --> Pointer) is native { * }

my $libm = dlopen($*VM.platform-library-name('m'.IO, :version(Version.new(6))).Str, 0x102);
if $libm.defined && +$libm != 0 {
    my $sym = dlsym($libm, 'cos');
    ok +$sym != 0, 'dlsym found a symbol';
    my $cos = nativecast(:(num64 --> num64), $sym);
    ok $cos ~~ Callable, 'nativecast to a Signature yields something callable';
    is-approx $cos(0e0), 1e0, 'the cast function pointer calls through to C';
} else {
    skip 'no versioned libm on this system', 3;
}

# vim: expandtab shiftwidth=4
