use Test;
use NativeCall;

# A typed `Pointer[T] $p is rw` out-parameter receives the address C wrote
# (Font::FreeType's `FT_Init_FreeType(Pointer[FT_Library] $lib is rw)`).

plan 4;

class Handle is repr('CPointer') { }
sub strtol(Str, Pointer[Handle] is rw, int32 --> long) is native {*}
sub strtol-untyped(Str, Pointer is rw, int32 --> long) is native is symbol('strtol') {*}

my $typed = Pointer[Handle].new;
is $typed.Int, 0, 'starts null';
is strtol('42abc', $typed, 10), 42, 'the call returns';
ok $typed.Int != 0, 'a typed Pointer out-parameter holds the written address';

my $plain = Pointer.new;
strtol-untyped('42abc', $plain, 10);
ok $plain.Int != 0, 'an untyped one still does';
