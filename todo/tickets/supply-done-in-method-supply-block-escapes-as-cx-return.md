# `done` in a `supply` block declared inside a method leaks CX::Return, killing the tap with a quit

## Affected tests

- `t/http2-response-serializer.rakutest` subtests 1, 2, 3 ("Header", "Header + Data", "Header + Data - Content-Length unspecified"): the ResponseSerializer transformer emits **no frames at all**, so `$test-completed` times out and all three `flunk`. The trigger is `Cro::HTTP2::ResponseSerializer.transformer`'s `react { whenever $resp.push-promises() { ... } }` (ResponseSerializer.rakumod line 50): for a non-2.0 response, `Cro::HTTP::Response.push-promises` (Cro::Core `lib/Cro/HTTP/Response.rakumod:129-133`) returns `supply { done }` **from a method**. Under mutsu the `done` unwinds as `CX::Return`, the react block dies, and the whole transformer supply quits before the Headers frame is emitted.

## Repro

`tmp/h2-react-done2.raku`, minimal variant:

```raku
class A {
    method pp() { supply { done } }
}
react { whenever A.new.pp() -> $x { say "got $x" } }
say 'ok';
```

- mutsu: `A react block: Died because of the exception: CX::Return` (exit 1)
- raku: `ok`

The same body in a **plain sub** works (`sub pp-sub() { supply { done } }` — passes), as does an inline `react { whenever (supply { done }) {} }`. Even a plain `.tap` on the method-returned supply delivers `quit => CX::Return` instead of `done` (`tmp/h2-react-done3.raku`).

Cro-level repro: `tmp/h2-resp-ser1.raku` (via `bash tmp/croflake.sh`, absolute `MUTSU_BIN`) prints `QUIT: A react block: ... CX::Return`; raku prints `GOT FRAME: Cro::HTTP2::Frame::Headers`.

## Root cause

1. The parser desugars a bare `done` inside a `supply` block into `$emitter.done(); return Nil` — `src/parser/primary/ident/supply.rs:131-140` (`Stmt::ReactDone => SyntheticBlock([MethodCall done, Stmt::Return(Nil)])`). The `Return(Nil)` is what terminates the on-demand body (Rakudo semantics: statements after `done` do not run).
2. That `Return` is an ordinary routine-return signal. When the on-demand lambda runs (at tap time), the return unwinds through the closure boundary logic in `src/vm/vm_closure_dispatch.rs:805-828`: for a non-routine closure, the return is **propagated to the lexically enclosing routine** whenever a target can be identified — `has_target = e.return_target_callable_id().is_some() || data.env.contains_key("__mutsu_callable_id")`. The code comment even names this exact case: "If no target exists (e.g., supply block done+return), catch it locally."
3. A closure created inside a **method** captures `__mutsu_callable_id` in its env (set when running the routine, see `src/runtime/builtins_operators_fallback.rs:579-590`), so `has_target` is true, the Return is stamped with the method's callable id and re-raised. The method frame returned long ago, so nothing consumes it and it escapes to the tap as an error → the supply quits with `CX::Return`. A closure created in a plain sub / mainline has no `__mutsu_callable_id` in env, hits the "catch locally" branch, and works — which is exactly the observed sub-vs-method asymmetry.

## Fix direction

Two options; A is surgical, B is the clean one:

- **Option A**: in `vm_closure_dispatch.rs` (the `!cc.is_routine` branch at ~line 809), treat an on-demand supply lambda as a local return boundary: if the executing closure's parameter is the supply emitter (name starts with `crate::parser::SUPPLY_EMITTER_PREFIX`, see `src/parser/mod.rs:17`; the param name is available on the compiled closure / SubData), catch the return locally instead of consulting `has_target`. The desugared `Return(Nil)` is the only `return` that can textually target the lambda itself, and a user-written `return` inside a supply block *should* target the enclosing routine only while that routine is still on the stack — Rakudo also treats escaping the frame as `X::ControlFlow::Return`, not a tap quit, so guarding this on the emitter-param prefix keeps user `return` semantics unchanged for the live-frame case.
- **Option B**: stop reusing `Stmt::Return` in the desugar. Give `supply.rs:131` a dedicated statement (e.g. `Stmt::SupplyBodyDone`) compiled to a control error that `run_on_demand_body` (`src/runtime/supply_promise.rs:93`) and the tap/react drive paths consume as normal termination (they already consume `is_react_done()` errors — but note the desugar deliberately does NOT use the react-done signal, because a bare `done` in a *supply* block must not terminate an enclosing react loop; the new control must be distinct from both `Return` and `ReactDone`).

Risk: the `done`-terminates-body semantics must be preserved (`supply { done; say "after" }` must not print "after" — pinned by comparing with raku), and react-block `done` (`Stmt::ReactDone` outside supply rewrite) must keep travelling to the react loop.

## Verification

- `tmp/h2-react-done2.raku`: all five variants print their `ok` lines, matching raku.
- `tmp/h2-react-done3.raku`: `.tap` fires the `done` callback, not `quit`; react completes.
- `raku -e`-vs-mutsu spot check: `my $s = supply { done; say "after done" }; $s.tap: {}, done => { say "DONE" }` → prints only `DONE`.
- `t/http2-response-serializer.rakutest`: 5/5 (currently 2/5). Note subtests 1-3 each have per-frame `ok $check($frame)` checks that will start executing; the file also depends on the slurpy ticket (`http2-slurpy-overflatten.md`) for those inner checks to iterate correctly, but plan/pass counts for the file's 5 named tests do not.
- `make test` + existing supply tests (`t/supply-*.t`, S17 whitelisted files are the regression surface for return/done semantics).
