# A `.share`d supply block starts however it is first consumed

`supply { whenever ... }.share` runs its block once, on the first tap, and every
later tap joins that run. mutsu only started the block when something called
`.tap` on the shared Supply itself. Every other first consumer read the shared
supplier directly and never started it: a `.grep`/`.map`/`.do`/`.head` on it, a
`whenever` inside another supply block or a react, and `.Promise`. So
`$shared.grep(* eq 'x').head(1).Promise` stayed `Planned` forever (#10740).

The "block already started" flag used to sit in the shared Supply's own
attributes, and only the receiver of a `.tap` call wrote it back. It is now
keyed on the shared supplier id (`native_methods/state_shared_supply.rs`). One
lock claims it, so the block runs exactly once whichever consumer arrives
first, from whichever thread. On top of that:

- `grep`/`map`/`do`/`head` on a shared supply derive an on-demand supply, as
  they already did for a plain `supply { }`. Its tap taps the shared one, so
  the start stays lazy, as in rakudo.
- A `whenever` (in a supply block or a react) and `.Promise` register on the
  shared supplier first and then start the block if it is still pending.
- A shared block started by a consumer that has no `done =>` callback still
  builds its done group, so its completion reaches every joined consumer.

The `Stomp` distribution surfaced this, and fixing it exposed two more bugs on
the same path:

- `body_reads_args_array` / `body_reads_args_hash` checked for an `@_` / `%_`
  read by searching `format!("{stmts:?}")`. A WhateverCode filter over a shared
  supply held in an attribute embeds a cyclic object graph, so the formatting
  recursed until the stack overflowed. Both now use the ADR-0137 visitor
  (`auto_signature_uses`).
- `while COND -> $x { given EXPR -> $m { } }` wrote `$m`'s final value back
  into `$x`. The `while` lowers to `while ($x = COND)`, and that assignment
  left a container-ref tag behind. The pointy `given` then took the tag as its
  own topic source. `OpCode::Given` now uses the register only when its own
  topic expression set the tag. Stomp's parse loop
  `while subparse(...) -> $/ { given $/.made -> $message { ... } }` lost `$/`
  to this.

Stomp's `t/client.rakutest` went from a timeout to 30 of 31 passing. The rest
is filed: closing one tap of a shared block tears it down for all taps
(#10831), a supply block's CLOSE phaser can't see the `my` variables declared
before it (#10832), and closing a supply-block tap does not run its `whenever`
source's `.on-close` (#10833, which blocks `t/server.rakutest`).
