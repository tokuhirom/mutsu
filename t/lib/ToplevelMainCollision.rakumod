# A package-less top-level `sub MAIN`, plus one nested in a `module { ... }`
# block -- mirrors roast/packages/HasMain/lib/HasMain.rakumod. Neither is
# `is export`, so neither should ever run, and neither should collide with a
# same-named top-level `sub MAIN` in the requiring/using scope: raku scopes a
# package-less top-level routine lexically to its own compilation unit.
module ToplevelMainCollision {
    sub MAIN() { say "should-not-run nested MAIN" }
}
sub MAIN() { say "should-not-run toplevel MAIN" }
