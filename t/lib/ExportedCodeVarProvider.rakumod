module ExportedCodeVarProvider {
    sub impl-a($x, $y) is export(:pack-raku) { "impl:$y" }
    our &unpack is export(:pack) = &impl-a;
}
