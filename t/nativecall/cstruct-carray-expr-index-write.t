use Test;
use NativeCall;

# Assigning into an index of a native CArray handle returned by an
# expression -- a sub call, a method call, an attribute accessor -- used to
# be a silent no-op (#8031): the write went into a throwaway Raku container
# instead of native memory. Only a handle already bound to a variable
# (`my $m = get(); $m[2] = 7;`) reached the write.

plan 4;

sub calloc(size_t, size_t --> Pointer) is native { * }
sub free(Pointer) is native { * }

my $arr = calloc(4, 4);
sub get() { nativecast(CArray[int32], $arr) }

get()[2] = 7;
is get()[2], 7, 'a sub-call-returned CArray handle writes through its index';

class Box { has $.c; }
my $b = Box.new(c => nativecast(CArray[int32], $arr));
$b.c[1] = 5;
is $b.c[1], 5, 'and so does an attribute-accessor-returned CArray handle';

# Binding first already worked before this fix; pin it alongside so a
# regression in either direction is caught by the same file.
my $m = get();
$m[0] = 9;
is $m[0], 9, 'a CArray handle bound to a variable still writes through';

# The two forms share the same underlying bytes.
is get()[0], 9, 'the sub-call and the bound-variable form see the same memory';

free($arr);
