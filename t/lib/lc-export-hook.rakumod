# A module with a lowercase name (like has-word) that hands its routine out
# through a `sub EXPORT` hook.
my sub lc-export-hook($a) { "lc:$a" }
my sub EXPORT { Map.new('&lc-export-hook' => &lc-export-hook) }
