# Soft `.UInt` Failures, assignment-statement sink semantics, and subset nominalization — http-router.rakutest reaches 82/83

Three related fixes, each verified against `raku` directly, that together let
`Cro::HTTP`'s `http-router.rakutest` test 83 ("Route with optional UInt named
arg for query parameter doesn't match negative values") pass — previously the
whole file aborted at that point. Combined with the `named_names` fix
(`news/2026-08/parameter-named-names-plain-named.md`), the file went 64/83 →
**82/83** in one day; the only remaining failure is the regex-alternation LTM
gap (`todo/deep/regex-alternation-ltm-longest-literal-prefix.md`).

## 1. Out-of-range `.UInt` returns an X::OutOfRange Failure

`"-1".UInt` / `(-5).UInt` threw immediately; rakudo returns an unthrown
`Failure` carrying `X::OutOfRange` (`what` "Coercion to UInt", `got`, `range`)
that only explodes when used or sunk. Fixed in
`dispatch_core_coerce.rs` following the existing `str_numeric_failure` shape.

## 2. Assignment statements are wanted, not sunk (`SinkPopAssign`)

Even with a soft Failure, `%h{$k} = "-1".UInt;` still exploded: the compiler
emitted `SinkPop` for the statement value, and sinking an unhandled Failure
throws. rakudo semantics (all verified): the bare element-assignment statement
and its `if`-modifier form stay soft; a topicalizing `with`/`given` modifier
DOES throw; `use fatal` throws in every form. A new `SinkPopAssign` opcode
covers `Expr::IndexAssign`/`MultiDimIndexAssign` statements (including behind
an `if` modifier), throwing only under `fatal_mode`.

## 3. Regex `{ ... }` code-block statements are wanted, not sunk

The third layer: Cro's generated route matcher runs
`%unpacks{Q[page]} = .UInt with $req.query-value(Q[page])` inside a regex code
block — a `with`-modifier form, which per rule 2 would throw. But rakudo
compiles regex code-block statements as wanted: the same statement that throws
at top level stays soft inside `/x { ... }/` (only a DESTROY-time warning).
The existing `in_regex_code_block` flag now guards the `SinkPop` Failure
throw.

## 4. `Parameter.type` nominalizes the builtin UInt subset

With the Failure flowing, the router still matched the route and hung: Cro's
`compile-route` only emits its signature-bind check (`<?{
$han.signature.ACCEPTS($cap) || ... }>`) when `$param.constraints` is
non-empty, and rakudo reports `UInt :$page` as `.type` `Int` +
`.constraints` `all(UInt)` — the subset is nominalized. mutsu reported
`.type UInt` / `all()`, so no bind check was generated and the Failure reached
the handler. `build_parameter_attrs` now nominalizes the builtin subset
(`UInt` → `Int`, subset moved into `.constraints`); with the bind check
generated, the negative value is rejected as NO MATCH → 404 (rakudo reaches
400 via `X::TypeCheck::Binding::Parameter` in the BIND-FAILS re-invoke; the
test only checks the empty body). User-declared subsets still need registry
plumbing — filed as
`todo/tickets/parameter-type-not-nominalized-for-user-subsets.md`.

Pins: `t/uint-coercion-failure.t` (14 tests: Failure shape, use/sink/fatal
throw matrix, regex-code-block softness) and `t/parameter-introspection.t`
(nominalized `.type`, subset in `.constraints`, plain type keeps `all()`).
