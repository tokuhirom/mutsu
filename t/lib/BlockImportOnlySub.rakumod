unit module BlockImportOnlySub;

# An `only` sub exported under a tag. Imported inside a nested block it must
# hide an outer `multi` of the same name for that block's extent.
sub ok(Bool $cond, Str $desc? = '') is export(:harness) {
    'imported only-sub ' ~ $cond
}

sub probe($x) is export(:harness) { "imported probe $x" }
