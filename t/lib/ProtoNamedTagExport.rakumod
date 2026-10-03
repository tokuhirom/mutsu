module ProtoNamedTagExport {
    our proto sub tagged-proto(|) is export(:S) {*}
    multi sub tagged-proto(Int $x) { $x * 2 }
    our sub plain-default() is export { 1 }
}
