# `.WHICH` on a user type object skips the method-call probe chain

A built-in method called on a receiver of a built-in shape (`$i.abs`,
`@a.elems`) is answered at the call site from the method table, past the ~30
probes of the full `CallMethodMut` path (ADR-11276). A type object or instance
of a user class, a role type object and `Any` have no shape, so `C.WHICH` ran
every probe before `Mu.WHICH` answered it: ~17k instructions per call, against
~6k for `5.WHICH`.

The call-site lane now answers `.WHICH`, `.defined` and `.DEFINITE` on such a
receiver too, with the verdict (the type declares or inherits no user method of
that name, and the row is pure) remembered per receiver type and registry
generation. `C.WHICH` drops from 17.3k to 6.2k instructions. Monad builds its
type-cache keys from `M.WHICH ~ '|' ~ T.WHICH` on every constructor call: one
iteration of `t/14_concurrent_types.t`'s chain loop goes from 9.59M to 9.30M
instructions (#17331).
