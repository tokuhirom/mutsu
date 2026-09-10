use Test;
use lib $?FILE.IO.parent.add('lib-cstruct-module').Str;
use NativeCall;
use CStructInModule;

# A `nativecast` handle must carry the class's **registered** name, and a
# CStruct class's own methods must be able to read their fields.
#
# Both halves are what `MoarVM::Guts::REPRs` needs: it declares
# `my class MVMArrayB is repr('CStruct')` inside a module and reads it with
# `method realstart(::?CLASS:D:) { +$!start ?? Pointer.new(…) !! $!any }`, so a
# short-named handle loses the method and a method that cannot read `$!start`
# is useless even when found.

plan 7;

sub calloc(size_t, size_t --> Pointer) is native { * }
sub free(Pointer) is native { * }

my $blk = calloc(1, 32);
ok $blk.defined, 'calloc gave us a block';

is body-class-name(), 'CStructInModule::Body',
    'the class knows its package-qualified name';

my $h = make-body($blk);
is $h.^name, 'CStructInModule::Body',
    'and a nativecast handle is tagged with it, not the short name';

# The generated accessor already worked; the hand-written method is the new part.
is $h.elems, 0, 'a generated field accessor reads the C struct';
is $h.describe, 'elems=0 start=0',
    'and a hand-written method reads its own fields with $!';

# Values C wrote are visible, and re-read on each call rather than cached — the
# reason the fields are materialised per method entry and not once.
sub memcpy(Pointer, Blob, size_t --> Pointer) is native { * }
memcpy($blk, Buf.new(7, 0, 0, 0, 0, 0, 0, 0), 8);
is $h.elems, 7, 'a field written behind mutsu is picked up by the accessor';
is $h.describe, 'elems=7 start=0', 'and by the method that reads $!';

free($blk);
