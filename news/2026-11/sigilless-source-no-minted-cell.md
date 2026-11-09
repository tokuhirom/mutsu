# An implicit-raw parameter does not box an unbound sigilless caller variable

Binding `Mu \candidate` to `hit` where `my \hit = $cache{key}` minted a shared `ContainerRef` for
`hit` and installed it under the caller's name, even though a sigilless variable owns no Scalar. A
later `candidate =:= Any` then compared a container Rakudo does not have and answered False for the
`Any` a cache miss produces, so Monad's type cache (`absent(hit) ?? publish(...) !! hit`) returned
`Any` as a cached type under four threads. The binder now mints no cell for a sigilless source
unless the argument already is that cell. Part of #17149; the SIGSEGV that remains under load is
still open there.
