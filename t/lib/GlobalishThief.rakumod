# Fixture for t/modules/import-export/globalish-merge-symbols-reexport.t
# (#15371): merges another unit's GLOBAL into its own at BEGIN time, as
# CompUnit::Util's `steal-globalish` does.
BEGIN {
    my $cu = $*REPO.need(CompUnit::DependencySpecification.new(:short-name<GlobalishSource>));
    GLOBALish.WHO.merge-symbols($cu.handle.globalish-package);
}
