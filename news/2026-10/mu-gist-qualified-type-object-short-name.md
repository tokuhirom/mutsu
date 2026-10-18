# `Mu::gist` on a namespaced type object answers the short name

`A::Foo.Mu::gist` (the idiom `multi method gist(::?ROLE:U:) { self.Mu::gist }` in
Hash::Agnostic / Map::Agnostic) returned `(A::Foo)`; Rakudo answers `(Foo)`. The `Mu.gist`
method row now shares `mu_gist_late` with the other type-object gist paths. Map::Match's
`t/01-basic.rakutest` now matches Rakudo file for file.
