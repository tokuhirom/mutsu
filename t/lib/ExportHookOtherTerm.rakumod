# Fixture for t/modules/import-export/export-hook-term-shadows-tagged-sub.t (#9339):
# an EXPORT hook that installs a DIFFERENT name, so the tag-exported sub is
# the only `u` there is.
sub u is export(:u) { "from-sub-u" }
sub EXPORT { %( 'other' => 42 ) }
