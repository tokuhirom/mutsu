use v6;
use Test;

# A type-constrained state scalar lives in a shared ContainerRef cell like
# untyped state scalars (#5959), with the constraint registered on the cell
# so the write chokepoint re-checks it. Plain `=` used to lose the write
# entirely (1,1 instead of 1,2) because typed scalars kept the plain store.
# Expected values verified against raku.

plan 6;

sub f() { state Int $n = 0; $n = $n + 1; $n }
is f(), 1, "typed state scalar first call";
is f(), 2, "typed state scalar accumulates across calls";

subset Small of Int where * < 10;
sub h() { state Small $n = 0; $n = $n + 1; $n }
h();
is h(), 2, "subset-typed state scalar accumulates";

sub k() { state Str $s = ""; $s ~= "a"; $s }
k();
is k(), "aa", "Str-typed state scalar accumulates with ~=";

sub bad() { state Int $n = 0; $n = "x"; $n }
throws-like { bad() }, Exception,
    message => /'expected Int'/,
    "assigning a wrong-typed value through the cell still dies";

# The buf/blob carve-out: a native-array-typed state scalar keeps the plain
# store so element assignment reaches the Buf (Digest's SHA2 shape).
sub buf-elem() { my $r = (state buf32 $w .= new); $w[0] = 42; $w[0] }
is buf-elem(), 42, "state buf32 element assignment still works";

done-testing;
