# Base64 bundled as the Base64-encoding battery

`Base64` (`github:ugexe`, v0.1.0, Artistic-2.0) is vendored at
`modules/Base64/` and resolves with zero config. Both upstream test files
pass, matching raku. It is a hard dependency of Cro::HTTP (WebSocket
handshake keys, basic-auth headers) — the third Cro::HTTP dependency locked
in behind the release gate, after `Crypt::Random` and
`IO::Path::ChildSecure`. (The already bundled `MIME::Base64` is a different
dist with a different API; Cro wants this one.)

The module is 45 lines of dense idiomatic Raku (`samewith`, `|c` captures,
`LAST` phasers, `rotor(:partial)`, `state` vars in expressions), and six
general interpreter fixes fell out; the vendored source is untouched:

- **Buf/Blob are Positional in list context** — `.rotor` and `for` over a
  Blob treated it as ONE item instead of iterating its bytes. Pin:
  `t/buf-positional-list-context.t`.
- **`@`/`%` parameter type constraints apply to the ELEMENTS** — the
  default in `Str:D :@alpha = @chars64std` was type-checked as a whole
  Array against `Str:D` and died with X::Parameter::Default::TypeCheck.
  Pin: `t/typed-aggregate-param-constraint.t`.
- **Multi dispatch on a typed named aggregate param** — `:alpha(@u)` never
  matched a `Str:D :@alpha` candidate: the named-param branch was
  sigil-blind, and the container-metadata comparison pitted the declared
  value type's type object against the `:D` smiley. Same pin.
- **`(my/state $x = init) op= rhs` leaked a VM stack slot** — the compile
  emitted a superfluous `Dup` on top of assign ops that already push their
  result, corrupting an enclosing expression's operands
  (`65 +< ((state $m = 24) -= 8)` computed `16 +< 16`). Pin:
  `t/paren-decl-compound-assign.t`.
- **A placeholder in a given/with body binds the TOPIC** — `do with EXPR
  { $^a ... }` bound `$^a` to the desugared defined-check's Bool instead
  (the shallow placeholder collector descended into Given bodies, which are
  their own placeholder scope). Fixed in both the statement and the
  do-expression compile paths. Pin: `t/given-with-placeholder-topic.t`.
- **Sub-form `grep` returns a Seq** like the method form, so the module's
  `--> Seq` return constraint passes. Pin: `t/grep-sub-returns-seq.t`.

Packaging: `batteries.lock` row + both files whitelisted in the release
gate, `t/base64-battery.t` smoke test, the selection record
`docs/batteries/base64.md`, the BATTERIES.md §7 index row, and a site row
via `scripts/gen-batteries-manifest.py`.
