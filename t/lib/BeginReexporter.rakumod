use nqp;
need BeginExportsSource;

# A BEGIN phaser of a module sees the compile-time resolver `$*R`; the unit's own
# EXPORT package is `$*R`'s `$!export-package` and merging another unit's
# EXPORT package into it re-exports that unit's symbols (CompUnit::Util's
# `re-export`, written with its own calls).
BEGIN {
    my $handle = $*REPO.need(CompUnit::DependencySpecification.new(:short-name<BeginExportsSource>)).handle;
    die 'no $*R at BEGIN time' unless $*R;
    die 'current-scope is not the unit' unless $*R.current-scope =:= $*CU;
    my \pkg = nqp::getattr($*R, RakuAST::Resolver, '$!export-package');
    pkg.WHO.merge-symbols($handle.export-package);
}

sub own is export { 'own' }
