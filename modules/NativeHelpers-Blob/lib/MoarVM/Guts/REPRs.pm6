use v6;

# This is ia module for access the guts of MoarVM's REPRs
# Right now lives here because it is incomplete, undocumented and is mainly a prof of concept
#
# When grow I'll move it to an independent module.

unit module MoarVM::Guts::REPRs:ver<0.0.3>;
use NativeCall;

constant ptrsize is export = nativesizeof(Pointer);
constant intptr is export = ptrsize == 4 ?? uint32 !! uint64;

constant Offset = do {
    my Pointer \p = Pointer.new(0xdeadbeaf); # A type with a trivial REPR
    my CArray[intptr] \ar = nativecast(CArray[intptr], Pointer.new(p.WHERE));
    my $i = 0;
    repeat { last if ar[$i] == p; } while ++$i < 10;
    die "Can't determine actual Offset" if $i == 10;
    $i * ptrsize;
};


# The body of the 'VMArray' REPR
my class MVMArrayB is repr('CStruct') {
    has uint64 $.elems;
    has uint64 $.start;
    has uint64 $.ssize;
    has Pointer $.any;

    method realstart(::?CLASS:D:) {
	+$!start ?? Pointer.new(+$!any + +$!start * ptrsize) !! $!any;
    }
}

# The body of the 'CArray' REPR
my class CArrayB is repr('CStruct') {
    has Pointer $.storage;
    has Pointer[Pointer] $.child;
    has int32 $.managed;
    has int32 $.allocated;
    has int32 $.elems;
}

# The body of the 'CStruct' REPR
my class CStructB is repr('CStruct') {
    has Pointer[Pointer] $.child_objs;
    has Pointer $.cstruct;
}

my %known-bodies = (
    VMArray => MVMArrayB,
    CArray => CArrayB,
    CStruct => CStructB
);

sub OBJECT_BODY(Mu \any) is export {
    Pointer.new(any.WHERE + Offset);
}

sub BODY_OF(Mu \any) is export {
    my \type = %known-bodies{any.REPR};
    die "Can only handle " ~ %known-bodies.keys if type ~~ Nil;
    nativecast(Pointer[type], OBJECT_BODY(any)).deref;
}
# vim: ft=perl6:st=4:sw=4
