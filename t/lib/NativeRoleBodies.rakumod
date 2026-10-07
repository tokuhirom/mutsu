unit class NativeRoleBodies;
use NativeCall;

# Twelve parametric roles whose body declares `is native` routines, the shape
# of NativeHelpers::CStruct's `LinearArray`.

role Alloc1[::T] {
    has @!cells handles <elems AT-POS>;
    has Pointer $!storage;
    sub calloc1(size_t, size_t --> Pointer) is native(Str) is symbol('calloc') { * }
    sub free1(Pointer) is native(Str) is symbol('free') { * }
    submethod BUILD(:$!storage!) { }
    method new(::?CLASS:U: Int $n) { self.bless(:storage(calloc1($n, 8))) }
    method live { $!storage.defined }
    method release { free1($!storage); $!storage = Pointer; True }
    method two { 2 }
}

role Alloc2[::T] {
    has @!cells handles <elems AT-POS>;
    has Pointer $!storage;
    sub calloc2(size_t, size_t --> Pointer) is native(Str) is symbol('calloc') { * }
    sub free2(Pointer) is native(Str) is symbol('free') { * }
    submethod BUILD(:$!storage!) { }
    method new(::?CLASS:U: Int $n) { self.bless(:storage(calloc2($n, 8))) }
    method live { $!storage.defined }
    method release { free2($!storage); $!storage = Pointer; True }
    method two { 2 }
}

role Alloc3[::T] {
    has @!cells handles <elems AT-POS>;
    has Pointer $!storage;
    sub calloc3(size_t, size_t --> Pointer) is native(Str) is symbol('calloc') { * }
    sub free3(Pointer) is native(Str) is symbol('free') { * }
    submethod BUILD(:$!storage!) { }
    method new(::?CLASS:U: Int $n) { self.bless(:storage(calloc3($n, 8))) }
    method live { $!storage.defined }
    method release { free3($!storage); $!storage = Pointer; True }
    method two { 2 }
}

role Alloc4[::T] {
    has @!cells handles <elems AT-POS>;
    has Pointer $!storage;
    sub calloc4(size_t, size_t --> Pointer) is native(Str) is symbol('calloc') { * }
    sub free4(Pointer) is native(Str) is symbol('free') { * }
    submethod BUILD(:$!storage!) { }
    method new(::?CLASS:U: Int $n) { self.bless(:storage(calloc4($n, 8))) }
    method live { $!storage.defined }
    method release { free4($!storage); $!storage = Pointer; True }
    method two { 2 }
}

role Alloc5[::T] {
    has @!cells handles <elems AT-POS>;
    has Pointer $!storage;
    sub calloc5(size_t, size_t --> Pointer) is native(Str) is symbol('calloc') { * }
    sub free5(Pointer) is native(Str) is symbol('free') { * }
    submethod BUILD(:$!storage!) { }
    method new(::?CLASS:U: Int $n) { self.bless(:storage(calloc5($n, 8))) }
    method live { $!storage.defined }
    method release { free5($!storage); $!storage = Pointer; True }
    method two { 2 }
}

role Alloc6[::T] {
    has @!cells handles <elems AT-POS>;
    has Pointer $!storage;
    sub calloc6(size_t, size_t --> Pointer) is native(Str) is symbol('calloc') { * }
    sub free6(Pointer) is native(Str) is symbol('free') { * }
    submethod BUILD(:$!storage!) { }
    method new(::?CLASS:U: Int $n) { self.bless(:storage(calloc6($n, 8))) }
    method live { $!storage.defined }
    method release { free6($!storage); $!storage = Pointer; True }
    method two { 2 }
}

role Alloc7[::T] {
    has @!cells handles <elems AT-POS>;
    has Pointer $!storage;
    sub calloc7(size_t, size_t --> Pointer) is native(Str) is symbol('calloc') { * }
    sub free7(Pointer) is native(Str) is symbol('free') { * }
    submethod BUILD(:$!storage!) { }
    method new(::?CLASS:U: Int $n) { self.bless(:storage(calloc7($n, 8))) }
    method live { $!storage.defined }
    method release { free7($!storage); $!storage = Pointer; True }
    method two { 2 }
}

role Alloc8[::T] {
    has @!cells handles <elems AT-POS>;
    has Pointer $!storage;
    sub calloc8(size_t, size_t --> Pointer) is native(Str) is symbol('calloc') { * }
    sub free8(Pointer) is native(Str) is symbol('free') { * }
    submethod BUILD(:$!storage!) { }
    method new(::?CLASS:U: Int $n) { self.bless(:storage(calloc8($n, 8))) }
    method live { $!storage.defined }
    method release { free8($!storage); $!storage = Pointer; True }
    method two { 2 }
}

role Alloc9[::T] {
    has @!cells handles <elems AT-POS>;
    has Pointer $!storage;
    sub calloc9(size_t, size_t --> Pointer) is native(Str) is symbol('calloc') { * }
    sub free9(Pointer) is native(Str) is symbol('free') { * }
    submethod BUILD(:$!storage!) { }
    method new(::?CLASS:U: Int $n) { self.bless(:storage(calloc9($n, 8))) }
    method live { $!storage.defined }
    method release { free9($!storage); $!storage = Pointer; True }
    method two { 2 }
}

role Alloc10[::T] {
    has @!cells handles <elems AT-POS>;
    has Pointer $!storage;
    sub calloc10(size_t, size_t --> Pointer) is native(Str) is symbol('calloc') { * }
    sub free10(Pointer) is native(Str) is symbol('free') { * }
    submethod BUILD(:$!storage!) { }
    method new(::?CLASS:U: Int $n) { self.bless(:storage(calloc10($n, 8))) }
    method live { $!storage.defined }
    method release { free10($!storage); $!storage = Pointer; True }
    method two { 2 }
}

role Alloc11[::T] {
    has @!cells handles <elems AT-POS>;
    has Pointer $!storage;
    sub calloc11(size_t, size_t --> Pointer) is native(Str) is symbol('calloc') { * }
    sub free11(Pointer) is native(Str) is symbol('free') { * }
    submethod BUILD(:$!storage!) { }
    method new(::?CLASS:U: Int $n) { self.bless(:storage(calloc11($n, 8))) }
    method live { $!storage.defined }
    method release { free11($!storage); $!storage = Pointer; True }
    method two { 2 }
}

role Alloc12[::T] {
    has @!cells handles <elems AT-POS>;
    has Pointer $!storage;
    sub calloc12(size_t, size_t --> Pointer) is native(Str) is symbol('calloc') { * }
    sub free12(Pointer) is native(Str) is symbol('free') { * }
    submethod BUILD(:$!storage!) { }
    method new(::?CLASS:U: Int $n) { self.bless(:storage(calloc12($n, 8))) }
    method live { $!storage.defined }
    method release { free12($!storage); $!storage = Pointer; True }
    method two { 2 }
}

method build() {
    (Alloc1[Int].new(3), Alloc2[Int].new(3), Alloc3[Int].new(3), Alloc4[Int].new(3), Alloc5[Int].new(3), Alloc6[Int].new(3), Alloc7[Int].new(3), Alloc8[Int].new(3), Alloc9[Int].new(3), Alloc10[Int].new(3), Alloc11[Int].new(3), Alloc12[Int].new(3))
}
