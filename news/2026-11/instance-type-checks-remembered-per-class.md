# A user-class instance's type checks are remembered per (class, type)

Whether an instance of a user class matches a nominal class or role type is
decided by the class's MRO and composed roles alone, but `$obj ~~ R` and every
declared-type check (`R $x` parameters, `--> R` returns, `my R $v`
assignments) re-derived it through the type-matching gauntlet on each call:
about 20k instructions for `$obj ~~ R`, 12k for a return-type check.

Both checks now consult a memo keyed by `(class, type, check)` and dropped on
every registry write. It is used only where nothing else can change the
answer: the receiver is a plain instance of a class the program declared, the
type is a registered class or role named without a parameterization (a `:D`
smiley is admitted for a declared type, an instance being definite), it is not
a subset or an enum, and at that point of the program its name is not
re-bound by a type capture, a package alias, or a constant or term of the same
name.

Monad's `is-some` / `is-right` and the `--> Monad::Maybe` return of every
constructor are this shape; one iteration of `t/14_concurrent_types.t`'s chain
loop went from 12.2M to 10.85M instructions (#17331).
