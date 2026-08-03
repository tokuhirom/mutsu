# A nested `whenever` keeps its own supply's emitter

`Cro::HTTP::Middleware::Request`-based middleware now reaches the client. With
`before-matched LowerCase; after-matched STS; delegate <*> => $application` in a
route block, mutsu's response is byte-for-byte raku's — the lowercased target
routes to the delegated application, the response carries the added header, and
the body arrives. Subtests 1 and 2 of the vendored Cro suite's
`http-middleware.rakutest` pass.

Two independent defects kept the value from flowing down the pipeline. Both are
the same underlying hazard as #5830/#5831: a `supply { … }` block's emitter is
named uniquely per *parse site* but shared by every runtime *instance* of that
site, so as soon as two instances of one block are live at once — precisely what
a middleware pipeline and a delegating route set build — the wrong instance can
answer an `emit`.

## A `whenever` nested in another `whenever` inherited no owned lexicals

`Cro::HTTP::Router::RouteSet.transformer` is

```raku
supply {
    whenever $requests -> $request {
        …
        whenever $handler.invoke($request, $args) -> $response {
            emit $response;
        }
    }
}
```

The outer `whenever` gets its owned-lexical set from the enclosing supply body's
`CompiledCode` (`exec_whenever_scope_op`), which is where the emitter name is
marked as belonging to *this* instance. The inner one cannot: it registers from
inside the outer callback's body, and that body is a chunk re-compiled from AST
that knows nothing about the supply block that created it — so its owned set came
out empty and `emit $response` re-resolved the shared emitter name against
whichever sibling instance happened to be dispatching it. With an outer route set
delegating to an inner one, the response ping-ponged between the delegate
pipeline's body-serializer stage and the outer route set forever.

A `whenever` callback already carries the supply body's vouched set in
`SubData::authoritative_captures`. That set now rides along into the chunk
`eval_block_value` compiles for the callback body
(`CompiledCode::inherited_owned_lexicals`, via
`Interpreter::pending_whenever_inherited_owned`), and `exec_whenever_scope_op`
seeds every nested registration's owned set from it. The vouch therefore cascades
to any depth of nesting rather than stopping at the first callback.

## `emit` used as a sub-expression was never rewritten

`supply { … }` lowers to `Supply.on-demand(-> $__mutsu_supply_emitter_N { … })`
and the parser rewrites `emit` in the body to `$__mutsu_supply_emitter_N.emit(…)`
— but only where `emit` was a whole *statement*. `Cro::HTTP::Middleware`'s role
writes it as a ternary arm:

```raku
supply whenever wrap-request-logging(self, $pipeline, { self.process($_) }) -> $request {
    $request ~~ Cro::HTTP::Request
        ?? emit($request)
        !! die "Request middleware {self.^name} emitted a $request.^name()…";
}
```

That `emit` stayed a bare call and fell back to the runtime's *dynamic* emitter
stack. Bare `emit` is dynamically scoped on purpose (raku prints `1, 2` for
`sub e($x) { emit $x }; supply { e(1); emit 2 }`), but here the dynamically
innermost emitter is a neighbouring pipeline stage's — so the request skipped the
rest of the pipeline and surfaced at the far end of the whole chain.

`rewrite_supply_stmt` now hands expression statements to a small expression
rewriter (`parser::primary::ident::supply_emit_expr`) that rewrites `emit` calls
anywhere in the expression tree. It stops at closure boundaries (`AnonSub`,
`Lambda`, `AnonSubParams`), where the dynamic stack is the right answer and where
rewriting would hit the closure-capture gap already documented on
`rewrite_supply_stmt`; inline blocks are handed back to the statement rewriter.
Unhandled `Expr` shapes are returned untouched, so the worst case is today's
dynamic behaviour rather than a miscompile.

Pinned by `t/supply-nested-whenever-emitter.t`.
