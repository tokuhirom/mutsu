# `do given X -> $p` binds its parameter; `$h<a b>:delete:kv` deletes

Two interpreter gaps found by making the Cookie::Jar distribution's suite pass.
`do given X -> $p { ... }` (the value form) never read the topic for its pointy
parameter, so `$p` was undefined; it now pushes the topic and restores the
outer `$_` like the statement form. A value-adverb subscript with `:delete`
(`$h<a b>:delete:kv`, `:delete:k`) over a scalar holding a Hash reported the
entries but left them in the hash; it now removes them. All five Cookie::Jar
test files pass.
