use v6.d;

proto sub unlink(|) is export {*}
multi sub unlink(--> Int:D) {
    &CORE::unlink(CALLER::LEXICAL::<$_>).elems
}
multi sub unlink(*@paths --> Int:D) {
    &CORE::unlink(@paths).elems
}
