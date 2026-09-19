unit module ProtoMultiMismatchedDeclaredName;

# An exported proto exports its later multi candidates as one family, even
# when the unit declaration's name differs from the path used by `use`.
proto sub mismatched-export($) is export {*}
multi sub mismatched-export(Int $value) { "int:$value" }
multi sub mismatched-export(Str $value) { "str:$value" }

proto sub mismatched-export-capture($object, |) is export {*}
multi sub mismatched-export-capture($object, UInt $length) { "uint:$length" }
multi sub mismatched-export-capture($object, Whatever) { "whatever" }
