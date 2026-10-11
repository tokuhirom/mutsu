use v6;
use Test;
use NativeCall;

# ADR-11827: the two upstream NativeCall shapes that depend on routine identity.

plan 8;

# 1. `is native is symbol(...)` on a sub: the symbol mixin and the native mixin land on
#    the one routine object, so Native!setup looks up the right symbol.
sub qsort(Pointer, size_t, size_t, &cmp (Pointer, Pointer --> int32))
    is native is symbol('qsort') { * }
sub cmp(Pointer $a, Pointer $b --> int32) {
    nativecast(CArray[int32], $a)[0] <=> nativecast(CArray[int32], $b)[0]
}
is &qsort.native_symbol, 'qsort', 'the symbol trait reaches the routine the native trait mixed into';
ok &qsort ~~ NativeCall::Native, 'the sub does Native';
my @a := CArray[int32].new(5, 3, 9, 1);
qsort(nativecast(Pointer, @a), 4, 4, &cmp);
is-deeply @a.list, (1, 3, 5, 9).Seq.list, 'qsort ran through the native symbol';

# 2. `is native` on a method: the `{ * }` body must not run and the installed method
#    is the native one.
class CStr-ish is repr('CPointer') {
    method len(--> size_t) is native is symbol('strlen') { * }
}
sub strdup(Str --> CStr-ish) is native is symbol('strdup') { * }
my $p = strdup("hello");
is $p.len, 5, 'an `is native` method calls the C function';
my $m = $p.^find_method('len');
ok $m ~~ NativeCall::Native, 'the installed method did Native';
is $m.native_symbol, 'strlen', 'the method carries its symbol';
is $p.len, 5, 'a second call dispatches the same native method';
ok $m.^name.starts-with('Method+'), 'the method is still a Method with mixins';

done-testing;
