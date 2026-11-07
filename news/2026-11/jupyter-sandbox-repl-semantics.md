# Four fixes behind Jupyter::Kernel's sandbox (t/02-sandbox 4 -> 28 of 54 ok)

- `$OUTERS::x = ...` written from a method whose parameter is also named `x` now reaches an outer
  `state $x` (it already reached `my $x`): `share_outer_written_decl` no longer skips `state`.
- `$_ = Nil` inside `given $x` / `for $x` decays to the SOURCE variable's `is default(...)`
  (`my $x is default(Nil)` stays `Nil`) instead of to `$_`'s own `Any`.
- With `$*OUT` replaced by an object, `say`/`put`/`print`/`note` (statement form in tail position
  and the routine form) answer what that object's `print` answered, so a marker role such as
  Jupyter's `True but role { method __hide }` survives (`OpCode::LoadOutputResult`).
- A parse failure carries rakudo's `X::Comp.pos` (character offset), which the vendored `REPL`
  needs to tell "input ended early" (`pos == $code.chars`) from a real error.
