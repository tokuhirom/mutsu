unit module LateImportProtoGreet::Inner;

proto late-proto(|c) is export { * }
multi sub late-proto(Str $kind, |c) is export { "proto-$kind" }
multi sub late-proto(Int $n) { $n * 2 }
