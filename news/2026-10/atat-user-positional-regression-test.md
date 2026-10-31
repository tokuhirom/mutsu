# `@@x` on a user Positional is pinned by a test

`@@buffer` on a `:=`-bound user class that does `Positional` answers the
object's `.list`, as rakudo does (#15286). The behaviour already held on `main`;
`t/collections/list-context-user-positional.t` now pins it, and annotations'
`t/01-containers.rakutest` passes every assertion on a release build (its
remaining difference is speed, #15833).
