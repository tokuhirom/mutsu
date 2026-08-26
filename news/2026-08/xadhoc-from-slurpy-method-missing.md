# `X::AdHoc.from-slurpy(...)` is implemented

Found by the doc-diff harness re-run (`Type/X/AdHoc.rakudoc:56`).

## What was wrong

`X::AdHoc.from-slurpy(...)` is a documented class method that builds an
`X::AdHoc` from a slurpy positional argument list. mutsu had no such method at
all — calling it threw `X::Method::NotFound`.

rakudo's definition stores the arguments as a `Capture` with a marker role
mixed in, and renders the message as the concatenation of their
stringifications:

```raku
try { X::AdHoc.from-slurpy( 3, False, "Not here" ).throw };
print $!.payload.^name; # Capture+{X::AdHoc::SlurpySentry}
print $!.message;       # 3FalseNot here
```

## The fix

Three pieces, all of them real rather than shaped to the example:

- **The method.** `runtime/methods_adhoc_slurpy.rs` builds the capture,
  composes the marker role onto it and stores it as `.payload`, with the
  concatenated message. It sits with the other native class methods on builtin
  type objects (`Promise.allof`, `Promise.in`, ...) because it is variadic and
  so does not fit the arity-keyed native method tables. Like rakudo, a
  `Failure` argument contributes the exception it carries rather than itself,
  so the message names the underlying error instead of "Failure".
- **The role.** `X::AdHoc::SlurpySentry` joins the `X::` role registry in
  `runtime_init.rs`, so the composition can happen at all and `~~` /`.^roles`
  agree with rakudo.
- **A general bug the example exposed.** `what_type_name` had no `Capture`
  arm, so a Capture fell through to the `_ => "Any"` default and *any*
  role-mixed capture reported the wrong name: `(\(1, 2) but R).^name` answered
  `Any+{R}` where rakudo says `Capture+{R}`. Fixed for every capture, not just
  this payload.

The doc's example now prints `Capture+{X::AdHoc::SlurpySentry}` and
`3FalseNot here`, matching `raku`.
