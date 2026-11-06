# A `gather` returned from a module resolves the names its own module imported

A `gather` whose body named a constant its module had `use`d (`sub g() is export { gather { take KK } }`)
resolved `KK` to `Any` once a caller that never imported it forced the list, because the body ran under
the forcing frame's compunit. The `gather` now records the compunit it was written in and each pull or
force runs its body under a block frame for that file, as a closure call does. Math::Sequences'
`lazy gather for 𝕀 -> $n { ... }` (imported range constant) is the shape that hit it (#16853).
