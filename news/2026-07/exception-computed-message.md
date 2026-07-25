# An exception that computes its message no longer reports `(Any)`

raku has one answer to "what does this exception say": `$exc.message`.
`Exception.message` merely *defaults* to the stored `$!message` attribute, so a
class that overrides it wins everywhere. The idiom is common — compute the text
from the exception's other attributes, cache it in the attribute:

```raku
class X::HTTP::Response is X::HTTP {
    has $.message;
    method message { $!message //= "Response error: '$.rc'" }
}
X::HTTP::Response.new(:rc('404 Not Found')).throw;
# was: (Any)
# now: Response error: '404 Not Found'
```

mutsu read the `message` ATTRIBUTE directly in every path that renders an
exception. The attribute exists but is undefined until the method runs, so it
stringified to the literal `(Any)`. Three separate mistakes fed into that:

- The `gist`/`Str`/`Stringy`/`message` arm in `methods_instance_ops.rs` answered
  from the attribute for any exception-ish instance. Its guard read
  `method != "message"`, with a comment claiming `message` only reached it when
  no user method existed — but user-method resolution happens ~300 lines later in
  the same function, so a user `method message` was unreachable through that
  path. The arm now falls through whenever the class defines the method itself.
- `.throw` derived its text from the attribute in three places. They now share
  one helper, `exception_message_text` (`runtime/exception_message.rs`), which
  asks the user method first, then the defined attribute, then `X::AdHoc`'s
  `payload`, then the class-specific formatted message.
- The pure-value native fast path cannot see the class registry at all, so it
  could not make that decision. `throw`/`rethrow`/`gist`/`Str`/`Stringy` on an
  exception whose message is computed (or absent) are now routed to the
  interpreter by the two bypass gates
  (`vm_native_dispatch::try_native_method` and
  `should_bypass_native_fastpath`), keyed on
  `exception_render_needs_interpreter` — which rejects ordinary instances on two
  cheap map/string checks before doing any registry work.

An exception with nothing to say now names its class the way raku does
(`Unthrown Empty with no message` unthrown, `Died with Empty` once thrown)
instead of rendering the undefined attribute as `(Any)`.

This took `HTTP::UserAgent`'s upstream suite 25/27 → 26/27: `t/082-exceptions`
passes, including its `throws-like … message => "Response error: '404 Not Found'"`
matcher. The one remaining failure, `t/040-request` subtest 18, now throws the
right exception and differs only in the type NAME it prints — see
`todo/tickets/subset-package-qualified-name.md`.

Pin: `t/exception-computed-message.t`.
