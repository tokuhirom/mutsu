# A binding failure now carries a real `Parameter` object

`X::TypeCheck::Binding::Parameter.parameter` stored the parameter's *name* as a
`Str`. That is enough to render the message, but raku exposes a `Parameter`
there, and code that recovers from a binding failure introspects it rather than
reading the text.

`Cro::HTTP::Router` is exactly such code. When no route matches, it re-invokes
each candidate whose signature rejected the capture and reads the resulting
exception to choose a status:

```raku
CATCH {
    when X::TypeCheck::Binding::Parameter {
        my $param = .parameter;
        if $param.named { $status = 400; last }
        elsif $param ~~ Cro::HTTP::Router::Auth || $param.type ~~ Cro::HTTP::Auth {
            $status = 401; last
        }
    }
    default {}
}
```

Under mutsu `.parameter` answered a `Str`, so `$param.named` died with `No such
method 'named' for invocant of type 'Str'` inside the router's supply — which
swallowed the response entirely. A request that should have been answered
`401 Unauthorized` got **no response at all**: the socket simply produced nothing.

`RuntimeError::with_parameter_object` now stamps the materialized `Parameter`
onto the exception, and the two constraint/type binding-failure sites in
`runtime/types/binding_signature.rs` (which already hold the `ParamDef`) use it.
The `Parameter` is built by the same
`crate::value::signature::make_parameter_value_from_param_def` that
`Signature.params` uses, so it is born as whatever mixin type a custom trait on
the declaration composed — which is how `$param ~~ Cro::HTTP::Router::Auth`
(from Cro's `is auth` trait) can ever be true.

## Result

Cro's `t/http-middleware.rakutest` is **fully green, 24 of 24 subtests** — the
first time that file has passed end to end. The last failure was the auth
scenario: an `Admin`-constrained route reached by a merely logged-in session must
produce 401, which an `after` middleware then rewrites into a redirect to `/401`.

Pinned by `t/typecheck-binding-parameter-object.t`.

Two named-parameter assertions were left out of that test on purpose: mutsu does
not check a named parameter's type constraint at all, so no exception is raised
to introspect. That gap is recorded in
`todo/tickets/named-parameter-type-constraints-are-not-enforced.md`.
