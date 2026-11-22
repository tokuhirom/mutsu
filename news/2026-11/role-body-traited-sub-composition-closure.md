# A traited role-body `sub` closes over its composition

A role body's plain `sub`s are declared again in each composing class's
package, closed over that composition's lexicals and parameters (#13957,
#15100). A `sub` with a custom trait was left out, so that running the trait
again would not apply it twice (#13645): it stayed the role's own
registration, closed over the body run's values, and read `Any` for a role
parameter or lexical (`my role P[$x] { sub h is probe { $x } }`, #18721).

Since a role-body sub's traits now run once and their result is recorded
(#18097), a traited sub is re-declared per composer too, without running its
traits again and with a mixin they left carried over -- unless the traits
wrapped the routine or rebound its body (a `.wrap`, NativeCall's `$!do`),
which belong to the role's one routine.
