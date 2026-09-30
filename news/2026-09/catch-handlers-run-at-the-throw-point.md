# CATCH handlers run at the throw point

Every `CATCH` handler now runs where the exception was thrown, before anything
unwinds, as rakudo does (ADR-0072 Slices 2 and 3, #9896). Before, only a
handler containing `.resume` ran there; every other one ran after the dying
routine's frames were gone.

What that changes, each pinned against `raku` in
`t/exceptions/catch-runs-at-throw-point.t`:

- A handler runs before the dying routine's `LEAVE` phasers and sees its
  dynamic variables, whether or not it resumes.
- The handlers of nested regions form a chain, innermost first. An inner
  handler that matches nothing or `.rethrow`s passes the exception on at the
  throw point, so an outer `.resume` resumes the original `die` — even when
  the inner `CATCH` sits in the same routine as the `die`.
- A `next` in a handler reaches the loop innermost at the `die`, as in rakudo;
  a `return` still returns from the routine that installed the handler.
- A handler reads its own lexicals, not same-named ones of the dying routine,
  and calls helpers private to its own package.

Fixed along the way: `eqv` called the right operand's user `.raku` even after
the left one had thrown.
