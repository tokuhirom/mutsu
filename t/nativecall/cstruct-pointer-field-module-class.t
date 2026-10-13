use Test;
use NativeCall;

plan 3;

# #12934: a pointer field of a CStruct declared inside a module reads back
# as a handle of the field's class under its registered (package-qualified)
# name, so the class's methods answer it. Font::FreeType's
# `FT_Size.face.FT_Reference_Face`: `FT_Face` is forward-declared as a
# CStruct inside `unit module Font::FreeType::Raw`.
module R::Raw {
    class Face is repr('CStruct') { ... }
    class Size is repr('CStruct') {
        has Face $.face;
        has int32 $.x;
    }
    class Face {
        has int32 $.n;
        method hello { "hello " ~ $!n }
    }
}

my $size = R::Raw::Size.new(face => R::Raw::Face.new(n => 7), x => 1);
my $read = nativecast(R::Raw::Size, nativecast(Pointer, $size));
is $read.face.^name, 'R::Raw::Face', 'the field reads back as the qualified class';
is $read.face.hello, 'hello 7', 'a method of the class answers the handle';
is $read.face.n, 7, 'the field value is read through the handle';
