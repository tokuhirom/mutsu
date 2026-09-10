use Test;

# NativeCall callback parameters (ADR-0063): a `&name (Sig)` or `& (Sig)`
# parameter of an `is native` sub accepts a Raku Callable and
# marshals it to a real C function pointer that re-enters the VM.
#
# Everything here goes through libc's `qsort` / `bsearch`, so the file needs no
# distribution and no library beyond the C runtime.

use NativeCall;

plan 13;

sub carray-list($a, $n) { (^$n).map({ $a[$_] }).List }

# --- The documented spelling: `&cmp (Sig)` with a space, per
#     Language/nativecall.rakudoc's "Function arguments" section. -------------
sub qsort_named(CArray[int32], size_t, size_t,
                &cmp (Pointer, Pointer --> int32))
    is native is symbol('qsort') { * }

sub deref-int32(Pointer $p) { nativecast(CArray[int32], $p)[0] }

my $calls = 0;
sub ascending(Pointer $x, Pointer $y --> int32) {
    $calls++;
    deref-int32($x) - deref-int32($y);
}

my $a = CArray[int32].new(5, 3, 9, 1);
qsort_named($a, 4, 4, &ascending);
is-deeply carray-list($a, 4), (1, 3, 5, 9),
    'a named &cmp (Sig) callback drives qsort';
ok $calls > 0, 'the Raku callback actually ran (an outer lexical saw it)';

# The comparator's own lexical state is the calling VM's state: re-entry shares
# the interpreter rather than running on a fresh clone.
my $before = $calls;
my $b = CArray[int32].new(4, 2, 8, 6);
qsort_named($b, 4, 4, &ascending);
is-deeply carray-list($b, 4), (2, 4, 6, 8),
    'the same callback works on a second call';
ok $calls > $before, 'the callback kept mutating the same outer lexical';

# --- The anonymous spelling `& (Sig)`, as LibZip's bindings write it. --------
sub qsort_anon(CArray[int32], size_t, size_t,
               & (Pointer, Pointer --> int32))
    is native is symbol('qsort') { * }

my $c = CArray[int32].new(30, 10, 20);
qsort_anon($c, 3, 4, &ascending);
is-deeply carray-list($c, 3), (10, 20, 30),
    'an anonymous & (Sig) callback parameter marshals too';

# --- A different comparator gives a different order, so the closure really is
#     the one that was passed (and not a cached first one). ------------------
sub descending(Pointer $x, Pointer $y --> int32) {
    deref-int32($y) - deref-int32($x);
}
my $e = CArray[int32].new(5, 3, 9, 1);
qsort_named($e, 4, 4, &descending);
is-deeply carray-list($e, 4), (9, 5, 3, 1),
    'a second, different callback is marshalled to its own function pointer';

# --- An anonymous lambda, not a named sub. ----------------------------------
my &lam = -> Pointer $x, Pointer $y --> int32 {
    deref-int32($x) - deref-int32($y);
};
my $f = CArray[int32].new(3, 1, 2);
qsort_named($f, 3, 4, &lam);
is-deeply carray-list($f, 3), (1, 2, 3),
    'an anonymous lambda works as a callback';

# --- A callback whose result is consumed by a native function returning a
#     Pointer: bsearch(key, base, nmemb, size, compar). ---------------------
sub bsearch_i32(CArray[int32], CArray[int32], size_t, size_t,
                &cmp (Pointer, Pointer --> int32))
    returns Pointer is native is symbol('bsearch') { * }

my $sorted = CArray[int32].new(2, 4, 6, 8, 10);
my $key    = CArray[int32].new(6);
my $hit    = bsearch_i32($key, $sorted, 5, 4, &ascending);
ok $hit.defined,             'bsearch found the key through the Raku callback';
is deref-int32($hit), 6,     'bsearch returned a pointer at the matching element';

my $missing = CArray[int32].new(7);
nok bsearch_i32($missing, $sorted, 5, 4, &ascending).defined,
    'bsearch reports a miss through the same callback';

# --- A callback that returns the comparison as an Order enum still marshals to
#     the C `int` the signature declares. ---------------------------------
sub order-cmp(Pointer $x, Pointer $y --> int32) {
    deref-int32($x) <=> deref-int32($y);
}
my $g = CArray[int32].new(11, 5, 8);
qsort_named($g, 3, 4, &order-cmp);
is-deeply carray-list($g, 3), (5, 8, 11),
    'an Order return value unboxes into the declared int32 result';

# --- A callback that itself makes a native call: the active-interpreter stack
#     has to nest (this is the libarchive shape, where the write callback runs
#     from inside `archive_write_data`). ---------------------------------
sub c_strlen(Str $s) returns int64 is native is symbol('strlen') { * }
my $nested = 0;
sub nesting-cmp(Pointer $x, Pointer $y --> int32) {
    $nested += c_strlen("abc");
    deref-int32($x) - deref-int32($y);
}
my $h = CArray[int32].new(9, 2, 5);
qsort_named($h, 3, 4, &nesting-cmp);
is-deeply carray-list($h, 3), (2, 5, 9),
    'a callback may itself make a native call';
is $nested % 3, 0,
    'the nested native call inside the callback returned its real result';
