# Helper for t/modules/import-export/trait-mod-is-export-symbol.t, reduced
# from List::AllUtils: re-export every routine of another module's
# EXPORT::all stash through CORE's `trait_mod:<is>(..., :SYMBOL, :export)`.
module SymbolExportRelay {
    use SymbolExportSource;
    sub local-routine { 'local' }
    BEGIN {
        for SymbolExportSource::EXPORT::all:: -> $stash {
            trait_mod:<is>(
              (SymbolExportRelay::{$stash.key} = $stash.value),
              :SYMBOL($stash.key),
              :export(:all)
            );
        }
        trait_mod:<is>(&local-routine, :SYMBOL('&renamed'), :export(:all));
    }
}
