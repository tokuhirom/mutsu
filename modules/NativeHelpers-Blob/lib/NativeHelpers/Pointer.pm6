use v6;

unit module NativeHelpers::Pointer:ver<0.1.1>;
use NativeCall;
use nqp;

NativeCall::Types::Pointer.^add_method('add', method (Pointer:D: Int $off) {
    my \type = self.of;
    die "Can't do arithmetic with a void pointer" if type ~~ void;
    my int $a = nqp::unbox_i(nqp::decont(self)) + $off * nativesizeof(type);
    nqp::box_i($a, Pointer[type]);
});

NativeCall::Types::Pointer.^add_method('succ', method (Pointer:D:) {
    self.add(1);
});

NativeCall::Types::Pointer.^add_method('pred', method (Pointer:D:) {
    self.add(-1);
});

multi sub infix:<+>(Pointer \p, Int $off) is export {
    p.add($off);
}
