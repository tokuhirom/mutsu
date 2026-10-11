# `$x does R(v)` finds a `my role` of the enclosing `unit class`

Inside a sub of `unit class M` loaded from another file, `$sub does Parsed($x)`
where `Parsed` is a `my role` of `M` was compiled as an ordinary coercion call and
died with "Impossible coercion from 'M' into 'M::Parsed'". The role is registered
under a package-qualified, mangled key (`M::Parsed\0<decl-id>`) and the sub's
frame has no binding for it, so the role lookup behind the `does`/`but`
initializer missed it. The lookup now also walks the package chain with the
mangled-key resolver. `Getopt::Long`'s `is getopt` trait works, and with it the
`App::Prove6` suite (`t/success.t`) passes under mutsu.
