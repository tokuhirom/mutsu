# This is a module for access the guts of MoarVM's REPRs
# Right now lives here because it is incomplete, undocumented and is
# mainly a prof of concept
#
# When grow I'll move it to an independent module.

unit module MoarVM::Guts::REPRs:ver<0.1.13>;
use NativeCall;

constant ptrsize is export = nativesizeof(Pointer);
constant intptr is export = ptrsize == 4 ?? uint32 !! uint64;

constant Offset = do {
    my Pointer \p = Pointer.new(0xdeadbeaf); # UNCOVERABLE
    my $offset;
    # The GC may move p at any allocation, leaving the address WHERE
    # returned pointing at stale memory. A scan only counts if p is still
    # at the same address afterwards, proving we scanned the live object.
    for ^1000 {
        my \where = p.WHERE;
        my CArray[intptr] \ar = # UNCOVERABLE
          nativecast(CArray[intptr], Pointer.new(where));

        my $i = 0;
        repeat { last if ar[$i] == 0xdeadbeaf; } while ++$i < 10; # UNCOVERABLE

        if $i < 10 && p.WHERE == where {
            $offset = $i * ptrsize; # UNCOVERABLE
            last;
        }
    }
    die "Can't determine actual Offset" without $offset; # UNCOVERABLE
    $offset;
};


# The body of the 'VMArray' REPR
my class MVMArrayB is repr('CStruct') {
    has uint64 $.elems;
    has uint64 $.start;
    has uint64 $.ssize;
    has Pointer $.any;

    method realstart(::?CLASS:D:) {
        +$!start
          ?? Pointer.new(+$!any + +$!start * ptrsize)
          !! $!any
    }
}

# The body of the 'CArray' REPR
my class CArrayB is repr('CStruct') {
    has Pointer $.storage;
    has Pointer[Pointer] $.child;  # UNCOVERABLE
    has int32 $.managed;
    has int32 $.allocated;
    has int32 $.elems;
}

# From Moar v2018.12+ the body of 'CStruct' REPR changed
my class CStructB is repr('CStruct') {
    has Pointer $.cstruct;
    has Pointer[Pointer] $.child_objs;  # UNCOVERABLE
}

my constant %known-bodies = (
    VMArray => MVMArrayB,
    CArray  => CArrayB,
    CStruct => CStructB
);

# Plain, GC-managed snapshots of the REPR bodies. A `.new` on a CStruct type
# mallocs a native buffer that is never freed when the object is collected, so
# returning an owned CStruct copy from BODY_OF would leak one buffer per call.
# These hold the same fields in ordinary attributes instead. The pointers
# copied out (storage, slots, cstruct) are not GC-managed and stay valid after
# the object moves.
my class MVMArrayBody {
    has $.elems;
    has $.start;
    has $.ssize;
    has $.any;

    method realstart() {
        +$!start
          ?? Pointer.new(+$!any + +$!start * ptrsize)
          !! $!any
    }
}

my class CArrayBody {
    has $.storage;
    has $.child;
    has $.managed;
    has $.allocated;
    has $.elems;
}

my class CStructBody {
    has $.cstruct;
    has $.child_objs;
}

proto sub snapshot($) {*}
multi sub snapshot(MVMArrayB \b) {
    MVMArrayBody.new(
      elems => b.elems, start => b.start, ssize => b.ssize, any => b.any)
}
multi sub snapshot(CArrayB \b) {
    CArrayBody.new(
      storage   => b.storage,
      child     => b.child,
      managed   => b.managed,
      allocated => b.allocated,
      elems     => b.elems)
}
multi sub snapshot(CStructB \b) {
    CStructBody.new(cstruct => b.cstruct, child_objs => b.child_objs)
}

sub OBJECT_BODY(Mu \any) is export {
    Pointer.new(any.WHERE + Offset);
}

sub BODY_OF(Mu \any) is export {
    my \type = %known-bodies{any.REPR};

    die "Can only handle " ~ %known-bodies.keys if type ~~ Nil;
    # A struct view over the body address goes stale as soon as the GC moves
    # the object, so snapshot the body into a plain object we own under an
    # address-stability check. A fresh CStruct copy would leak. See the body
    # snapshot classes above for why a plain snapshot does not.
    for ^1000 {
        my \where = any.WHERE;
        my \body  =
          snapshot(nativecast(Pointer[type], Pointer.new(where + Offset)).deref);
        return body if any.WHERE == where;
    }
    die "Can't read object body";
}

# vim: expandtab shiftwidth=4
