# A type capture is read from its binding, not resolved as a bare word

`T` inside `sub f(::T $x)`, or inside a method of `role R[::T]`, used to
compile to `GetBareWord`, so every read walked the whole bareword resolution
chain (enum members, module-scope terms, package type lookups, ...) before
reaching the `env` entry the binder had just written. In Monad's
`method some($value) { some-type(self.maybe-type, T).new(:$value) }` that one
read cost about 68k instructions per call.

The compiler now emits `GetTypeCapture` for a bare word that names the
routine's own capture or its role's type parameter (unless a same-frame local
or lexical type of that spelling is nearer). The op pushes the `env` binding
while its `TypeCaptureBound` marker is live, and falls back to the bareword
resolution otherwise (a role method run on the bare role type object).

`Monad::Maybe.some(1)`: 690k -> 622k instructions per call (callgrind,
profiling build, 200 calls, warm). Refs #17331.
