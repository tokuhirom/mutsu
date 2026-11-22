# A Failure answers its own methods as Rakudo's Failure does

`Failure`'s own methods are now rows with the semantics of Rakudo's
`src/core.c/Failure.rakumod` (ADR-11276). A method that needs the value throws
an unhandled `Failure`; once handled, the method answers a defused value.

- Throwing a Failure marks it handled, as Rakudo's `!throw` does: `.handled`
  stays `False` no longer after `$f.lines` or `$f.Int` threw it.
- `.mess` is the message, a newline and the backtrace of the `fail` site, with
  `(HANDLED) ` in front once defused. It used to be `X::Method::NotFound`.
- `.backtrace` is that `Backtrace`. It used to be `Any`.
- `.gist` and `.Str` answer `.mess` once handled and throw an unhandled Failure.
  Before, `.gist` of an unhandled Failure answered its message, and `.Str` of a
  handled one threw.
- Once handled, `.Int` is the `Int` type object, `.Num` and `.Numeric` are `NaN`,
  and `.Set`/`.Bag`/`.Mix` and their `Hash` forms hold the Failure. All of them
  threw the wrong exception before.
- `.AT-POS`, `.AT-KEY` and `.Failure` answer the Failure itself, and a handled
  Failure's `.sink` answers `Empty`.

The interpreter's call dispatch and the VM's two method-call opcodes each had
their own copy of the "an unhandled Failure explodes" check, with lists of
exempt methods that had drifted apart. There is now one entry,
`failure_own_method`, which asks the rows first.
