# A module whose `sub EXPORT` rejects an unknown symbol name with `die`, as
# BinaryHeap's does.
sub EXPORT(**@keys) {
    my %EXPORT = ::<FailingExportHook>.WHO;
    if my @missing = @keys.grep({ %EXPORT{$_}:!exists }) {
        die "not exported: " ~ @missing.join(", ");
    }
    Map.new(@keys.map({ $_ => %EXPORT{$_} }))
}

unit module FailingExportHook;

our sub fehook-answer is export { 42 }
