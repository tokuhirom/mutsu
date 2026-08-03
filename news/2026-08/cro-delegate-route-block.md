# `delegate` in a Cro route block works

`route { delegate <*> => $inner }` produced no response at all: the client's
`await Cro::HTTP::Client.get(...)` resolved to `Any` and, with a plain
`Cro::Transform` as the delegate target, the request never reached the
transform's `transformer` either. This blocked the `before-matched` /
`after-matched` subtests of the vendored Cro::HTTP `t/http-middleware.rakutest`,
which wrap an application by delegating to it.

Two independent interpreter bugs were behind it.

## `Parameter.constraints` is an `all()` junction

Cro's route compiler decides whether a compiled route needs a signature bind
test by asking each parameter for its constraints and counting them:

```raku
sub extract-constraints(Parameter:D $param) {
    my @constraints;
    sub extract($v --> Nil) { @constraints.push($v) }
    extract($param.constraints);
    return @constraints;
}
```

That works because Raku's `.constraints` is an `all()` junction: an
unconstrained parameter yields the *empty* `all()`, calling `extract` on it
autothreads zero times, and `@constraints` stays empty. mutsu returned
`Bool::True` for an unconstrained parameter, so every parameter looked
constrained. A wildcard `delegate`'s handler signature is `-> *@ { }`, and the
spurious "has constraints" verdict made the generated route regex carry a
`$han.signature.ACCEPTS($cap)` bind check that Rakudo does not emit.

`.constraints` is now always a `Junction` — `all()` when the parameter has no
value constraint, `all($literal)` for a literal parameter, `all($code)` for a
`where` clause. The empty junction still smartmatches truely against anything,
so its use as a matcher is unchanged. With this, mutsu's generated route matcher
is byte-identical to Rakudo's for the same route block.

## `supply` works as a statement prefix

`Cro::HTTP::Router::RouteSet::DelegateHandler.invoke` opens its pipeline with

```raku
my $current = supply emit $req;
```

mutsu only understood `supply { ... }` and the `supply whenever ... { ... }`
shorthand, so this was a parse failure ("Two terms in a row") raised when the
method ran — on a supply worker, where it was swallowed and left the pipeline
silently dead. The related `supply for ^3 { emit $_ }` parsed but ran its body
outside any supply, so the `emit` escaped as an unhandled `CX::Emit`.

`supply STATEMENT` now lowers exactly like `supply { STATEMENT }`, following the
same statement-prefix path `gather` already used: the statement is parsed with
`statement_pub`, its terminating `;` is given back to the caller, and a
`do`-block statement is inlined rather than nested.

Pinned by `t/parameter-constraints-junction.t` and
`t/supply-statement-prefix.t`.
