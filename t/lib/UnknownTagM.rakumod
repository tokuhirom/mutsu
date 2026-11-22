# Fixture for t/modules/import-export/import-unknown-tag-beside-all.t: one
# DEFAULT export and one tagged :B.
sub ut-foo is export { "foo" }
sub ut-bar is export(:B) { "bar" }
