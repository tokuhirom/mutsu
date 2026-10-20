# `Raku.version` on the type object, and `Kernel`'s attribute errors

`Raku.version` is the language revision in Rakudo, so the type object answers
it (`v6.d`, or `v6.e` under `use v6.e.PREVIEW`); mutsu refused it as an
attribute read. `Kernel.name` and the other `Kernel` attributes on the type
object answered `X::Method::NotFound` because mutsu's `Kernel` class declared
no attributes; they now raise Rakudo's "Cannot look up attributes in a Kernel
type object", and `Kernel.signals` is refused for its `:D` invocant (#13952).
