# `Nil`'s own methods are method-table rows

The methods Rakudo declares on `Nil` itself (`src/core.c/Nil.rakumod`) are now
rows of the method table (ADR-11276), each with Rakudo's signature, instead of
arms of the VM's `nil_predispatch_verdict` cascade, which knew only some of them:

- all 25 `Cool` text methods "that should just warn" answer `""` with
  `Use of Nil.<name> coerced to empty string`. `indices`, `substr-eq`, `tclc`,
  `wordcase`, `trans`, `indent`, `subst` and `substr-rw` used to answer `Nil`
  silently. `chars`/`lc`/`uc`/... take no positional argument, as in Rakudo;
- `Nil.Int(|)` warns with arguments too, and `Nil.Str(1)` fails its signature
  (`X::Multi::NoMatch`) instead of answering `Nil`;
- `STORE` throws `X::Assignment::RO` ("Cannot modify an immutable Nil value"),
  `BIND-POS`/`BIND-KEY` answer an `X::Bind` `Failure` and `ASSIGN-POS`/`ASSIGN-KEY`
  die with `Use of Nil.<name> not allowed`. Before, all five died with a
  made-up "Invocant ... must be an object instance" message. The two-argument
  forms that bind `Any`'s narrower candidates behave as in Rakudo;
- `Nil.Set`/`SetHash`/`Bag`/`BagHash`/`Mix`/`MixHash` hold the one `Nil`
  (`(Nil=>1).Bag`) instead of being absorbed into `Nil`.

A hyper call over a `Nil` element (`(Nil, "ab")».chars`) reaches the same rows.
