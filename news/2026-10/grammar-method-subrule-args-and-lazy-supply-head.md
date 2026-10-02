# Grammar method subrules with arguments, and lazy `head` over `supply` blocks

Found by working the `Stomp` distribution.

- `<.method(args)>` inside a grammar token never called the method when it had arguments, so
  `<.malformed('invalid command')>` silently failed to match instead of raising. A method subrule
  now receives its arguments and its `die` propagates out of `.parse`, as in Rakudo.
- `.head(N)` on a `supply { whenever ... }` block ran the block eagerly and blocked until its
  30-second drain deadline. It is now a derived on-demand supply that taps the source per tap of its
  own, passes the first `N` values on, then signals done.
- `.Promise` on a derived on-demand supply (`.grep`/`.map`/`.head` over a `supply` block) was kept
  immediately with `Nil`; it now stays planned until the supply is done and carries the last value.

`Stomp`'s `t/parser.rakutest` now passes in full. `t/client.rakutest` and `t/server.rakutest` still
need grep/map/head over a `.share`d on-demand supply to start it (#10740).
