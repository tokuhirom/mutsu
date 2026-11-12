# Fixture for t/modules/import-export/exporthow-merged-at-begin.t (#17557):
# a routine that, called from a module's BEGIN, merges another unit's
# `EXPORTHOW` into the calling unit's own, as CompUnit::Util's
# `re-exporthow` does.
use experimental :rakuast;
sub merge-exporthow(Str $name) is export {
    my $handle = $*REPO.need(
        CompUnit::DependencySpecification.new(:short-name($name))).handle;
    my $pkg := Metamodel::PackageHOW.new_type(:name<EXPORTHOW>);
    $*CU.merge-generated-lexical-declaration:
        RakuAST::Declaration::Import.new(
            :lexical-name<EXPORTHOW>, :compile-time-value($pkg)),
        :resolver($*R), :force;
    $pkg.WHO.merge-symbols($handle.export-how-package);
}
