# An error raised by a coercion method is no longer swallowed

`Target($value)` falls back to Raku's coercion protocol when the target type
declares no `COERCE`/`new`: it calls `$value.Target`. mutsu probed for that
method with a plain "did the call succeed?" test:

```rust
if args.len() == 1
    && let Ok(res) = self.call_method_with_values(args[0].clone(), name, vec![])
{
    return Ok(res);
}
// ... otherwise: "Impossible coercion from '<type>' into '<Target>'"
```

so an error raised *inside* a coercion method that does exist was discarded and
replaced with a coercion complaint. Two things were wrong with the report:

- the failure it named was not the failure that happened, and
- the source type it named was `Any` for **every** class instance, because
  `value_type_name` returns a `&'static str` and answers a flat `"Any"` for
  `ValueView::Instance`.

Debugging a Cro chunked response body therefore produced
`Impossible coercion from 'Any' into 'Promise': no acceptable coercion method
found` for what was really `No such method 'data' for invocant of type 'Buf'`
raised inside `Supply.Promise` — a message that named neither the real error nor
even the real source type, and sent the investigation down the wrong path
entirely.

## Fix

- Distinguish "the invocant has no `.Target` method" from "the `.Target` method
  it does have failed". The new `RuntimeError::is_method_not_found_for(name)`
  checks that the missing method is the one we asked for — a plain
  `is_method_not_found()` would still have swallowed the Cro case, whose real
  error was itself an `X::Method::NotFound` for a *different* method (`data`).
  Anything else propagates unchanged.
- Report the value's real type in `X::Coerce::Impossible`. The new
  `types::diagnostic_type_name` answers an instance's class name and a type
  object's package name, falling back to `value_type_name` otherwise, so
  `Promise(Plain.new)` now says `from 'Plain'` exactly as raku does (previously
  `from 'Any'`).

Pinned by `t/coercion-method-error-propagates.t`, whose four assertions match
`raku` exactly: an `X::AdHoc` from the coercion method propagates, an
`X::Method::NotFound` raised *inside* it propagates, a value with no coercion
method still gets `X::Coerce::Impossible`, and that message names the source
type.
