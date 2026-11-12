# Fixture for t/modules/import-export/export-hook-generated-lexicals.t (#15371):
# a `sub EXPORT` that merges declarations into its importer the way
# CompUnit::Util's `set-unit` / `set-lexpad` do: into `$*CU` (the importing
# unit) and into `$*R.current-scope` (the block holding the `use`).
use experimental :rakuast;

sub EXPORT {
    $*CU.merge-generated-lexical-declaration:
        RakuAST::Declaration::Import.new(
            :lexical-name<unit-term>, :compile-time-value('from-unit')),
        :resolver($*R), :force;
    $*R.current-scope.merge-generated-lexical-declaration:
        RakuAST::Declaration::Import.new(
            :lexical-name<pad-term>, :compile-time-value('from-pad')),
        :resolver($*R), :force;
    Map.new
}
