# Role methods take the fast method binder; `ObjAt` and `--> Mu` stop paying a dispatch

Every method of a parametric role whose body could see role context (`T`,
`::?CLASS`, a role-body lexical) was refused by the fast method binder and took
the full binder, about 14k instructions more per call: the fast binder had no
step that installs a role's composition bindings. It now runs the same steps as
the full binder, from one shared helper (`vm_method_role_bindings.rs`), and keeps
those names out of its exit merge. `$d.t` for `role P[::T] { method t { T } }`
went from 23.8k to about 10k instructions.

Two smaller costs on the same path went too: an `ObjAt` (`.WHICH`) operand of
`~` / `eq` is read by the `ObjAt.Str` row's own function instead of a full
method dispatch (16.3k to ~3k instructions per `$w ~ '|'`), and a `--> Mu`
return type, which admits everything, no longer resolves and checks the value.

Monad builds every value through such methods and keys its type cache by
`M.WHICH ~ '|' ~ T.WHICH`; one iteration of `t/14_concurrent_types.t`'s
`check-chains` loop went from 15.8M to 14.4M instructions with the first change
alone (#17331).
