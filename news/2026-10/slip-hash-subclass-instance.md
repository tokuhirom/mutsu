# `|$obj` of an `is Hash` subclass slips its pairs

`|$d` where `$d` is an instance of a class that `is Hash` used to slip the
object itself as one positional argument, so `f(|$d)` with `sub f(:$a)` died
with "Too many positionals". Rakudo slips the pairs, which become named
arguments, exactly as `|%h` does. PDF's `PDF::IO::Filter.encode` passes its
`PDF::COS::Dict` this way (`self!encode-item($input, |$dict)`), so encoding a
stream died. An `is Array` subclass instance slips its elements the same way.

On the way, a private method called on a type object with arguments that fail
to bind now reports the binding failure. It used to report "No such private
method", which is what the instance path already avoided (#12907).
