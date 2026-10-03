# Fixture for t/modules/import-export/export-sub-trait-mod-dispatcher.t.
# The trait candidate is a `multi` lexical to `sub EXPORT`, handed to the
# importer only through its dispatcher -- upstream NativeCall's shape (#11530).
our module ExportSubTraitDispatcher {
    our @APPLIED;
}

sub EXPORT(|) {
    my $t := multi trait_mod:<is>(Routine $r, :$xdtrait!) {
        @ExportSubTraitDispatcher::APPLIED.push: $r.name;
    };
    Map.new('&trait_mod:<is>' => $t.dispatcher);
}
