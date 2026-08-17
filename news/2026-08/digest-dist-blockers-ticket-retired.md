# Digest dist blocker ticket retired — bundling campaign is complete

`todo/tickets/digest-dist-blockers.md` tracked the blockers found while bundling grondilu's
`Digest` distribution (`Digest::MD5`, `Digest::SHA1`, `Digest::SHA2`, `Digest::SHA3`,
`Digest::RIPEMD`, `HMAC` — Artistic-2.0). The dist has been bundled since 2026-08-05
(`modules/Digest/`, `docs/batteries/digest.md`,
`news/2026-08/digest-battery-bundled.md`), and every genuine *blocker* — seven initial general
interpreter bugs, four more from roast fallout, four behind a wrong MD5 digest, the RIPEMD
per-process/per-block state issue, the SHA512/SHA384 saturation bug, the SHA3 named-multi and
`samewith`-in-lazy-gather dispatch bugs, and the HMAC named-parameter narrowness gap — has been
fixed (see the individual `news/2026-08/digest-*.md` and related entries this ticket accumulated
links to over its life). `t/md5.t`, `t/ripemd.t`, `t/sha.t`, and `Digest::SHA3`'s own suite all
pass in full against the bundled copy.

Re-verified 2026-08-16/17 in the same pass: the `Digest::HMAC` proto/builtin-name collision this
ticket also tracked (`call-hash(&p1)` falling through to the `hash` builtin instead of dispatching
the passed `proto` sub) is fixed too, apparently as a side effect of unrelated work — no specific
commit was identified, but the exact repro now resolves correctly against `main`.

What the ticket still listed as open was never a `Digest` blocker — the dist doesn't hit any of
these paths — but each is a real, general interpreter gap discovered along the way. Rather than
carry them forward bundled under a stale "Digest" ticket name (they have nothing to do with
`Digest` itself), each was split into its own focused ticket, per the `todo/README.md` one-file-
per-finding convention:

- `todo/tickets/with-statement-modifier-hides-placeholders.md` — a `with` statement modifier
  hides the modified statement's own placeholders (`$^a`), the same class of bug the `given`
  modifier already had fixed.
- `todo/tickets/wide-buffer-bit-accessor-width-divergence.md` — `read-ubits`/`write-bits` bit
  offsets diverge from MoarVM on a buffer whose element width is greater than one byte
  (width-1 buffers, i.e. every practical use, are unaffected).
- `todo/tickets/package-qualified-proto-dispatch-blocks-gather-forcing.md` — a package-qualified
  call to a module's `proto` sub fails from outside the module, which in turn blocks a `gather`
  created inside a module routine from being force-resolved correctly when forced from the
  calling script's top-level scope. Not independently re-verified this round — flagged in the new
  ticket as needing a fresh repro before design work starts.

The `say`-swallows-an-exception-from-`.gist` issue this ticket also referenced was already fixed
separately on 2026-08-06 (`fix(runtime): say propagates an exception raised while computing
.gist`); the stale reference is simply dropped along with the rest of the ticket.
