# A method that `does` a role with `CALL-ME` runs that `CALL-ME` when called

AccessorFacade's `is accessor-facade(&get, &set)` trait does `$r does
Provider[&getter, &setter]` on the Method object, where `Provider` has a
`CALL-ME` returning a `Proxy`. Rakudo then invokes the method through that
`CALL-ME` with the invocant first; mutsu kept running the declared (empty)
body. After a method's traits have run, a composed `CALL-ME` is now bound as the
candidate's body (the innermost wrap-chain entry, as a `$!do` rebind is), a
closure forwarding its whole argument list to it. `AccessorFacade`'s
`t/010-compose.t` passes 29/29 as on rakudo. Fixes #15127.
