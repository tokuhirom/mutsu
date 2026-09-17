# XDG::GuaranteedResources: three general interpreter bugs, red -> green

Locked and worked via the ecosystem distribution roulette (board:
[tokuhirom/mutsu#7884](https://github.com/tokuhirom/mutsu/issues/7884)).
`XDG::GuaranteedResources` 2.0.0 went from `red` (0/1 baseline files, 4/10
assertions) to `green` (1/1 baseline files, 10/10 assertions), via three
unrelated general interpreter fixes, each pinned with a focused `t/` test.

## 1. `IO::Path.parent` cycled instead of climbing past the current directory

`path-to-directory-array` in the distribution's own source repeatedly calls
`.parent` on a relative `IO::Path` until it reaches `.`. Climbing past that
point (`.` -> `..` -> `../..` -> ...) is a normal thing for user code to do,
but mutsu's `.parent` collapsed a path whose basename was `..` back to the
*previous* level instead of stacking another `..`, so it cycled forever
between `".."` and `"../.."` — "Too deep recursion (out of stack space)".

The fix has to be careful: stacking another `..` is only correct once the
path is *nothing but* `..` segments (`..`, `../..`, ...). A `..` trailing a
real name (`foo/..`, `/foo/..`, `../a/..`) still just strips its ordinary
dirname (`foo`, `/foo`, `../a`) — an overly broad first attempt at this fix
broke exactly those cases in `roast/S32-io/io-path-unix.t`, caught by
`make roast` before publishing.

## 2. Multi-dispatch specificity ignored `::` namespaced type names

`constraint_base_name()` — used to derive the plain type name a multi
candidate's constraint is ranked against — stopped scanning at the first
`:` byte, which is also the first byte of a `::` package separator. So
`"Foo::Base"` and `"Foo::Derived"` both truncated down to `"Foo"`,  making
any two multi candidates that differ only in a namespaced subclass compare
as equally specific, with the base class winning regardless of declaration
order.

This bites a *builtin* case too: `IO::Path does Cool`, so
`File::Directory::Tree`'s `rmtree(Cool:D)` / `rmtree(IO::Path:D)` pair
always dispatched to the `Cool:D` candidate — which itself calls
`rmtree($path.IO)` — recursing through the same wrong candidate forever.

## 3. A `CONTROL` block unconditionally absorbed `return`

A `CONTROL` block observes control exceptions like `return`/`next`/`last`/
`warn` via `when`/`default` clauses, but only *handles* the signal when one
of those actually matches — an unmatched block must let the signal continue
untouched. That rule was already correctly implemented for next/last/warn/
etc., but missing entirely from the `return` arm: any `return` unwinding
through a `CONTROL` block's lexical scope was unconditionally turned into
`Nil`, whether or not the block matched anything. Every
`guarantee-resource`-style sub that returns a value from a body that also
declares an unrelated `CONTROL` block (there for a `CX::Warn`) silently
returned `Nil`.

## Tests

- `t/nativecall/native-io-path-lexical.t` — `.parent` stacking and the
  pure-dotdot-vs-real-segment distinction.
- `t/routines/dispatch/multi-namespaced-class-specificity.t` — namespaced
  multi specificity, including the `IO::Path`/`Cool` builtin case.
- `t/exceptions/control-block-unhandled-return.t` — unmatched vs matched
  `CONTROL` around a `return`, plus the dead-return-still-throws case.
