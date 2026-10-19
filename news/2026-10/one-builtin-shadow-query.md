# One shadow query for every built-in method entry

Before a built-in method row answers a call, mutsu asks whether something the
program declared comes first: a user method or accessor, a role mixed into the
receiver, an `augment` or `.wrap` of a built-in type, or a `FALLBACK` /
`handles *` for a `Cool`-only name. That question had five copies, one per
dispatch entry: the interpreter's native bypass, its by-name dispatchers, the
VM's native entry, and the `CallMethod` and `CallMethodMut` lanes. The copies
had drifted. The VM counted a public accessor only for a quoted name. The
interpreter's native entry never saw an `augment` of the receiver's built-in
type. Only some copies knew that an augmented slurpy-only `multi` ranks after
the built-in method.

`Interpreter::user_shadows_builtin` is now the one answer they all read
(ADR-11276 §2.3, §2.4). The receiver-state half of the old interpreter bypass
(a lazy `Seq` body, a computed exception message, a `Cool` name the receiver
lacks) is a separate guard, `native_receiver_defers`. This retires
`should_bypass_native_fastpath`, one of the deletions in slice 5 of the method
table campaign, together with its observational E4b shadow probe.
