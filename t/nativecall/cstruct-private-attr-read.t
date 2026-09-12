use Test;
use NativeCall;

# A private `$!field` on a CStruct handle used to read Nil instead of native
# memory (#8030): the public accessor (`$obj.field`) already went through
# `cstruct_field_value` (it compiles to a method call), but `$!field` compiles
# to a direct local-slot read, which had no CStruct fallback and always saw
# the instance's (empty) Raku attribute map.

plan 4;

sub calloc(size_t, size_t --> Pointer) is native { * }
sub free(Pointer) is native { * }

class S is repr('CStruct') {
    has int32 $!a;
    has int32 $.b is rw;
    method peek() { $!a }
}
class Flat is repr('CStruct') { has int32 $.a is rw; has int32 $.b is rw }

my $blk = calloc(1, 32);
my $s = nativecast(S, $blk);
is $s.peek, 0, 'a private scalar attribute reads zeroed native memory, not Nil';

nativecast(Flat, $blk).a = 17;
is $s.peek, 17, 'and a write through a different handle to the same bytes is visible';

nativecast(Flat, $blk).a = -5;
is $s.peek, -5, 'a private int32 read is signed, matching the public accessor';

# The public half already worked before this fix; pin it alongside so a
# regression in either direction is caught by the same file.
is $s.b, 0, 'the public accessor still reads the same native memory';

free($blk);
