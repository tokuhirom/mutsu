# Fixture for t/modules/import-export/export-stash-expression-bind.t (#15371):
# exports bound into an export tag's stash through an expression, the way
# CompUnit::Util's `set-in-WHO` walks `EXPORT::` at BEGIN time.
BEGIN {
    my $WHO := EXPORT::;
    $WHO.package_at_key('DEFAULT') unless $WHO<DEFAULT>:exists;
    $WHO<DEFAULT>.WHO<&stash-exported> := sub stash-exported { 'from the stash' };
    $WHO.package_at_key('extra') unless $WHO<extra>:exists;
    $WHO<extra>.WHO<&tagged-exported> := sub tagged-exported { 'tagged' };
}
