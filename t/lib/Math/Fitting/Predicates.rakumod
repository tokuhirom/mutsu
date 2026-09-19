unit module Math::Fitting::Predicated;

proto sub is-positional-of-lists($object, |) is export {*}
multi sub is-positional-of-lists($object, UInt $length) { True }
multi sub is-positional-of-lists($object, Whatever) { False }
