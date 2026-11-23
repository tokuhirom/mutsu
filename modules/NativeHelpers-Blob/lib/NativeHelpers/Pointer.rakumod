unit module NativeHelpers::Pointer:ver<0.1.13>;

use NativeCall;

NativeCall::Types::Pointer.^add_method('add', method (Pointer:D: Int $off) {
    my \type = self.of;
    die "Can't do arithmetic with a void pointer" if type ~~ void;

    use nqp;
    my int $a = nqp::unbox_i(nqp::decont(self)) + $off * nativesizeof(type);
    nqp::box_i($a, Pointer[type])
});

NativeCall::Types::Pointer.^add_method('succ', method (Pointer:D:) {
    self.add(1)  # UNCOVERABLE
});

NativeCall::Types::Pointer.^add_method('pred', method (Pointer:D:) {
    self.add(-1)  # UNCOVERABLE
});

multi sub infix:<+>(Pointer \p, Int $off) is export {
    p.add($off)
}
