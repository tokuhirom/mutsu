# `.new` on a mixin type object remembers that it is the default constructor

`.new` on a role-mixed type object (`Base but Role`, Monad's
`parameterized(M, T).^pun but Monad::Maybe::Some`) takes the default
constructor directly when nothing in the base's MRO or the mixed-in roles
declares `new` or `bless`. That check re-walked the MRO and probed every role
on each construction (~11K instructions); the verdict is now remembered per
type object and registry write generation, so a `new` added later still
claims the call. One Monad `t/14` chain iteration went from 8.15M to 7.87M
instructions (#17331).
