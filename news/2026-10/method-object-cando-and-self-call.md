# Method objects: `.cando`, `self` on call, and built-in `find_method` names

Taking JSON::RPC's server suite from red to 29/29 under mutsu fixed three method-object gaps:

- `Method.cando(\(Invocant, args))` on a `.^find_method` / `.^lookup` object returns the
  candidates that accept the capture.
- Calling a `Method` object with an explicit invocant (`$m($obj, 7)`) re-enters ordinary
  method dispatch, so `self` and `$.attr` work in the body.
- `.^find_method` resolves a user-defined method named like a metamodel method (`can`, `isa`,
  ...) first, and otherwise returns a `Method`/`Routine` object instead of a bare `Str`.

The remaining client-side residue (assignments through an itemized hash read from an array
element) is tracked as #10601.
