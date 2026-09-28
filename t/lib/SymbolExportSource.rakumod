# Helper for t/modules/import-export/trait-mod-is-export-symbol.t: the
# routines SymbolExportRelay re-exports under their stash keys.
module SymbolExportSource {
    our proto sub proto-routine(|) is export(:all) {*}
    multi sub proto-routine(@values) { "proto:" ~ @values.elems }
    multi sub proto-routine(Str $s) { "proto-str:$s" }
    our sub plain-routine(@values) is export(:all) { "plain:" ~ @values.elems }
}
