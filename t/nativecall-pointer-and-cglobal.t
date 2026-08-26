use v6;
use Test;
use NativeCall;

# NativeCall typed pointers and `cglobal`, pinned against real `raku`.
#
# Four gaps closed here:
#
# 1. A `--> Pointer[T]` native RETURN produced an opaque handle tagged with the
#    literal class name "Pointer[T]", which resolved neither `.of` nor `.deref`
#    ("No such method 'deref'").
# 2. `.raku` rendered the named-argument form (`.new(address => N)`) with an
#    unqualified type parameter; Rakudo renders `.new(N)` positionally and
#    qualifies the parameter (`Pointer[NativeCall::Types::void]`).
# 3. `Pointer[Str].deref` read the address as element 0 of a `CArray[Str]` --
#    i.e. as a `char**` -- and SEGFAULTED. `.deref` is `nativecast(.of, self)`
#    in Rakudo, and is now defined that way here too.
# 4. An UNDEFINED library name (`is native(Str)` / `Str.&cglobal(...)`) means
#    "this process's own symbol space"; `cglobal` stringified the type object
#    into the file name `lib(Str).so` and tried to dlopen it.
#
# Only libc/POSIX symbols present on both Linux and macOS are used, and
# everything allocated is freed.

plan 24;

sub strdup(Str $s --> Pointer[Str]) is native { * }
sub malloc(size_t $n --> Pointer[void]) is native { * }
sub free(Pointer $p) is native { * }

# --- 1. a `--> Pointer[T]` return is a TYPED pointer ------------------------

my $dup = strdup("Success!");
ok $dup.defined, 'strdup returns a defined Pointer';
ok $dup.of === Str, 'a Pointer[Str] return remembers its type parameter';
is $dup.deref, 'Success!', '.deref reads the NUL-terminated string it points at';
is $dup.^name, 'NativeCall::Types::Pointer[Str]',
        '.^name carries the parameterisation';
free(nativecast(Pointer, $dup));

# The same object built by `nativecast` must behave identically -- the two
# construction paths used to disagree.
my $again = strdup("Twice!");
my $cast = nativecast(Pointer[Str], nativecast(Pointer, $again));
is $cast.deref, 'Twice!', 'nativecast(Pointer[Str], ...) derefs the same way';
is $cast.^name, $again.^name, 'and reports the same type name';
free(nativecast(Pointer, $again));

# --- 2. .raku / .gist rendering --------------------------------------------

my $mem = malloc(32);
ok $mem.defined, 'malloc returns a defined Pointer[void]';
is $mem.^name, 'NativeCall::Types::Pointer[NativeCall::Types::void]',
        'a NativeCall type parameter is fully qualified in the name';
is $mem.raku, $mem.^name ~ '.new(' ~ $mem.Int ~ ')',
        '.raku renders the POSITIONAL constructor form';
like $mem.gist, /^ 'NativeCall::Types::Pointer[NativeCall::Types::void]<0x' <[0..9a..f]>+ '>' $/,
        '.gist prefixes the parameterised name';
free($mem);

my $null = Pointer.new(0);
is $null.raku, 'NativeCall::Types::Pointer.new(0)',
        'an untyped Pointer renders positionally too';
is $null.gist, 'NativeCall::Types::Pointer<NULL>', 'a NULL Pointer gists as <NULL>';
is Pointer.raku, 'NativeCall::Types::Pointer', 'the bare type object renders as its name';
is Pointer[void].raku, 'NativeCall::Types::Pointer[NativeCall::Types::void]',
        'a parameterised type object qualifies its parameter';
is Pointer[Str].^name, 'NativeCall::Types::Pointer[Str]',
        'a non-NativeCall parameter is left unqualified';

# --- 3. cglobal against the process-global namespace ------------------------

constant NOLIB = Str;

my $malloc-sym = NOLIB.&cglobal('malloc', Pointer);
ok $malloc-sym.defined,
        'cglobal with an undefined Str library resolves in the process namespace';
ok cglobal(Str, 'free', Pointer).defined, 'and so does the direct call form';
ok (try { cglobal(Str, 'mutsu_no_such_symbol_at_all', Pointer).Int }).defined.not,
        'a missing symbol still fails';

# `is native(<undefined>)` is the same rule on the sub-declaration side.
sub strlen-nolib(Str $s --> size_t) is native(NOLIB) is symbol('strlen') { * }
is strlen-nolib('hello'), 5, 'is native(Str) binds against the process namespace';

# --- 4. library specifier forms ---------------------------------------------

# `('name', version)` is the documented ABI/API-version spelling; the version
# has to reach the file name, which a failed lookup reports verbatim. The
# message is inspected directly rather than through `throws-like`'s
# `message => /.../` matcher because two separate substrings have to be
# present; the matcher itself is no longer skipped for X::AdHoc (see
# news/2026-08/throws-like-named-matchers-no-longer-silently-skipped.md).
constant MISSING = ('mutsu_no_such_lib', v99);
sub missing-listform() is native(MISSING) { * }
try missing-listform();
my $list-err = $!.message;
ok $list-err.contains('mutsu_no_such_lib') && $list-err.contains('99'),
        'a (name, version) List library spec reaches the loader with its version';

sub missing-twoarg() is native('mutsu_no_such_lib', v98) { * }
try missing-twoarg();
my $two-err = $!.message;
ok $two-err.contains('mutsu_no_such_lib') && $two-err.contains('98'),
        'and so does the two-argument is native(name, version) form';

# --- 5. lowercase repr classes and unparameterised CArray fields ------------

# libarchive declares `class archive is repr('CPointer')`: a LOWERCASE opaque
# handle. Passing one to a second native sub used to skip native registration
# entirely, leaving the `{ * }` stub body -- whose `Whatever` then failed the
# sub's own `--> int32` return check.
class fh is repr('CPointer') { * }
sub tmpfile(--> fh) is native { * }
sub fclose(fh --> int32) is native { * }

my fh $tmp = tmpfile();
ok $tmp.defined, 'a lowercase repr(CPointer) return is a defined handle';
is fclose($tmp), 0, 'and passing it to a second native sub calls through';

# `Compress::Zlib::Raw`'s `z_stream` has `has CArray $.next-in` -- a BARE,
# unparameterised CArray, which is one pointer in C. Missing it aborted the
# whole struct layout, which surfaced as `nativesizeof` calling the class a
# P6opaque.
class WithBareCArray is repr('CStruct') {
    has CArray $.buf;
    has int32  $.len;
    has long   $.total;
}
class WithPointer is repr('CStruct') {
    has Pointer $.buf;
    has int32   $.len;
    has long    $.total;
}
is nativesizeof(WithBareCArray), nativesizeof(WithPointer),
        'a bare CArray CStruct field is laid out as one pointer';

done-testing;
