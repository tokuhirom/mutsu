unit module ManualExportStashVarMod;

# A value placed directly in a tagged EXPORT stash is an export even without
# an `is export` declaration. The tag is seeded by a normal export, as in
# Interval's `:refine` hook.
sub seed is export(:special) { Nil }
BEGIN EXPORT::special::<Answer> := 42;
