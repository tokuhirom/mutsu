unit module ImportedProtoShadow;
my proto sub thing(|) is export {*}
multi sub thing(Str() $s) { "inner3(" ~ $s ~ ")" }
