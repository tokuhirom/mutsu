# A method on a defaulted parametric role type object takes its pun from either call entry

A method called on a parametric role's type object whose type parameters all
have defaults (`role P[::T = Any] { ... }; P.m`) runs on the pun of the
default parameterization. Only the container-aware `CallMethodMut` entry knew
that; a call compiled to the plain `CallMethod` entry -- an element read such
as `@roles[0].m` -- composed a pun named after the role itself and withdrew it
again on every call, at ~830k instructions a call.

Both entries now ask one helper, which also remembers the redirect per role
and method under the registry write generation, so the role probes, the
method walk and the default parameterization's materialization run once.
`@roles[0].m` drops from ~833k to ~81k instructions per call, and `P.m` from
~42k to ~37k (#17331, #17408).
