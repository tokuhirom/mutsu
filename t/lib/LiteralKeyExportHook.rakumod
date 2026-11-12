# A module whose `sub EXPORT` hands out routines named only by literal pair
# keys, built by a helper it calls (the shape of Red's migration artifacts).
sub EXPORT(Str $kind = '') {
    LiteralKeyExportHook::builders($kind)
}
module LiteralKeyExportHook {
    our sub builders(Str $kind) {
        Map.new(
            '&config' => -> &block { "config($kind): " ~ block() },
            '&make-table' => -> &block { "table: " ~ block() })
    }
}
