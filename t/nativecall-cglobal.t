use Test;
use NativeCall;

# `cglobal($libname, $symbol, $target-type)` reads a library's exported
# (`extern`) variable. It is NOT a Raku builtin — Rakudo exports it from
# `NativeCall.rakumod` — so it arrives with `use NativeCall`, and it returns a
# `Proxy` that "redirects all its accesses" (Language/nativecall.rakudoc), i.e.
# re-reads the symbol on every fetch.

plan 10;

# It DEREFERENCES: the symbol's address is where the variable lives, and the
# value is read from there. `optind` is glibc's getopt cursor, 1 before any
# parsing. (Checked against Rakudo, which answers 1 here too.)
is cglobal('libc.so.6', 'optind', int32), 1,
    'cglobal reads the value at the symbol, not its address';

# A pointer-typed global comes back as a Pointer holding what it points at.
my $environ = cglobal('libc.so.6', 'environ', Pointer);
isa-ok $environ, Pointer, 'a Pointer-typed global reads back as a Pointer';
isnt $environ.Int, 0, 'and environ is not NULL in a live process';

# A missing symbol or library throws, which is what makes the common
# existence probe work -- `NativeLibs::Searcher` finds a versioned shared
# object with exactly this shape, and through it `DBIish`'s mysql and Pg
# drivers locate their client libraries.
nok (try cglobal('libc.so.6', 'mutsu_no_such_symbol_xyzzy', Pointer)).defined,
    'an unknown symbol throws';
nok (try cglobal('libmutsu_no_such_library.so.99', 'printf', Pointer)).defined,
    'an unknown library throws';
ok ((try cglobal('libc.so.6', 'optind', Pointer)) ~~ Pointer),
    'the `(try cglobal(...)) ~~ Pointer` probe answers True for a real symbol';

# The Proxy is the contract, not an implementation detail: every read must
# re-fetch, which is the whole point for a variable C keeps changing. Bind it
# (`:=`) to keep the Proxy rather than a snapshot, as the docs' `errno` example
# does.
my $opt := cglobal('libc.so.6', 'optind', int32);
is $opt, 1, 'a bound cglobal reads through';
is $opt, 1, 'and reads through again';
throws-like { $opt = 5 }, Exception, 'writing to a C global is NYI, as in Rakudo';

# It is a module export, so it is a real routine and not ambient syntax.
ok defined(&cglobal), 'cglobal is a callable routine';
