unit module DeferredCStruct::Arr;
use NativeCall;

# The shape of NativeHelpers::CStruct's LinearArray: a parametric role whose
# body computes the element stride once (`my int $sol`) and whose methods close
# over it. The pin: those role-body lexicals must survive past the frame that
# ran the composition (a `require` inside a method).
role Linear[::T] is export {
    my int $sol = nativesizeof(T);
    my \ty = T;

    has Pointer $!storage;
    has @!cache handles <AT-POS elems>;
    has Int $!size;

    sub calloc(size_t, size_t --> Pointer) is native(Str) { * }
    sub free(Pointer) is native(Str) { * }

    submethod BUILD(:$!size!, :$!storage!) {
        @!cache := Array[ty].new(:shape($!size));
        for ^$!size {
            @!cache[$_] = nativecast(T, Pointer.new(+$!storage + $_ * $sol));
        }
        self;
    }

    method new(::?CLASS:U: Int $size) {
        self.bless(:$size, :storage(calloc($size, $sol)));
    }

    method stride() { $sol }
    method elem-type() { ty.^name }

    method dispose(::?CLASS:D:) {
        with $!storage {
            free($!storage);
            $!storage = Pointer;
        }
    }
}
