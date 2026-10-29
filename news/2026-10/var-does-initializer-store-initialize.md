# `my %h does R = ...` calls the role's STORE with `:INITIALIZE`

The initializer of `my %h does R = ...` / `my @a does R = ...` is the container's
initialization, so a role `STORE` now receives it with `:INITIALIZE` (as `my %h is R = ...`
already did). A later plain assignment still passes none. The parser marks the declaration's
`does` expression (`BinaryForm::DoesDeclInit`) and the compiler emits the one-shot initializer
mark the mixin container store consumes. This lets WriteOnceHash's `t/01-basic.rakutest`
(`my %owoh{Any} does WriteOnce = 42 => "a"`) get past its declaration.
