# HTML::Tag's test suite passes: immutable Map misses, role-parameter recursion, stash contributions

The ecosystem distribution `HTML::Tag` (all four `t/` files) now passes under mutsu. Three
general interpreter gaps were behind it:

- **A missing key on an immutable `Map` is `Nil`**, not `Any` (a plain `Hash` keeps `Any`). In
  `when %constant-map{$_}` the `Any` type object smartmatched every topic, so every attribute
  rendered as a bare self-resolving name (`<p id>`).
- **A role method recursing into another instance of the same role with a different argument
  clobbered the caller's `$T`.** The method-exit env merge copied the callee's role-parameter
  binding back into the caller's frame; role parameters are now frame-local like params and
  locals. `<p>x<a>y</a></a>` is now `<p>x<a>y</a></p>`.
- **A module's own `class Ns::x` declarations show up in the `Ns::` stash** even when the module
  does not declare `Ns::<its name>`. Only the types it pulled in transitively stay hidden (the
  `S10-packages/precompilation.t` rule), so `Ns::{$name}` finds them.
