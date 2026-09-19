# A grammar's `method TOP` override no longer breaks `.parse`/`.subparse`

`Grammar.parse(...)` and `.subparse` only recognized a start rule declared
with `rule`/`token`/`regex TOP`. A grammar that instead overrides `TOP` as a
plain `method` -- a well-established idiom for running setup code (binding a
dynamic variable from named args, say) before delegating to the real entry
rule via `self.<rule>` -- made `.parse` fail outright with
`X::Method::NotFound: Unknown method value dispatch (fallback disabled):
parse`, before `TOP` was ever invoked.

`dispatch_package_parse` now falls back to calling the start rule as an
ordinary method when it resolves to a plain user method rather than a
token/proto definition (`src/runtime/methods_grammar_method_start.rs`). The
call runs with `self` bound to a cursor over the whole input, anchored at the
requested start position -- the same cursor shape the NQP cursor protocol
builds -- so a delegating call back into the grammar (`self.some-rule`)
continues from the real parse position instead of restarting on `""`. That
fallback lived in `methods_instance_ops.rs` (the "grammar token called as an
instance method" path) and previously always subparsed against an empty
string regardless of the receiver; it now reads the receiver's own
`orig`/position when it is a live cursor, leaving the `G.new.tok`
truthiness-check case (a plain, non-cursor instance) unchanged.

The method's return value is required to be a Match/Cursor, matching
rakudo's own `Method 'TOP' returned a Nil object (Nil) rather than a Match
object` error when it is not; a full `.parse()`/`.parsefile()` still
requires the returned match to consume the whole input, same as a
regex-shaped start rule.

This was the last remaining root cause behind most of the `IP::Addr` zef
distribution's red baseline files -- its `IPv4-Grammar` uses exactly this
`method TOP (Bool :$validate = False) { ...; self.ip-variants }` idiom.

Pinned by `t/grammar/grammar-method-shaped-top.t`.

Closes #8752.
