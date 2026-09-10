use Test;
use NativeCall;

# `CArray[T].new` — and ONLY CArray — flattens an Array/List argument into its
# elements. `Array.new(@a, 3)` / `List.new(@a, 3)` keep `@a` as ONE element.
# (Verified against rakudo.)
#
# The idiomatic way to build a NUL-terminated C string depends on the flatten:
#   CArray[uint8].new($path.encode.list, 0)
# Without it that produced a 2-element buffer (the whole list as element 0,
# then 0), so the callee saw a garbage filename — OpenSSL's
# `use-client-ca-file` reported "No such file or directory" for a file that
# was right there.

plan 8;

my @a = 1, 2;

# CArray flattens.
is CArray[uint8].new(@a, 3).elems, 3, 'CArray[uint8].new flattens an Array arg';
is CArray[int32].new(@a, 3).elems, 3, 'CArray[int32].new flattens an Array arg';
is CArray[uint8].new(@a.list, 3).elems, 3, 'CArray.new flattens a List arg';

# Array / List do NOT flatten — the arg stays a single element.
is Array.new(@a, 3).elems, 2, 'Array.new does NOT flatten an Array arg';
is List.new(@a, 3).elems, 2, 'List.new does NOT flatten an Array arg';

# The C-string idiom yields the bytes plus the NUL terminator, in order.
my $c = CArray[uint8].new("t/x".encode.list, 0);
is $c.elems, 4, 'encoded bytes plus the NUL terminator';
is (^$c.elems).map({ $c[$_] }).join(','), '116,47,120,0', 'bytes are in order and NUL-terminated';

# Flattening composes with the already-flattened kinds (Range/Seq).
is CArray[uint8].new(1..3, 4).elems, 4, 'a Range arg still flattens alongside';

# vim: expandtab shiftwidth=4 ft=perl6
