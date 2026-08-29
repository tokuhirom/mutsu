# A package-less, non-exported top-level `sub helper` -- lexically scoped to
# this compunit in raku. It must not collide with (or overwrite) a same-named
# top-level sub in the requiring/using scope, and must not leak into that
# scope as a bare, callable name when there was no such collision.
sub helper() { "module helper" }
