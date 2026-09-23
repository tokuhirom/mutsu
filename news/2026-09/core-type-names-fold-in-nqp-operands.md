# CORE type names in `nqp::` operands fold to their type objects at parse time

`nqp::create(IterationBuffer)`, `nqp::bindattr(@r, List, '$!reified', $b)` and
`nqp::create(Uni)` looked their type name up at run time, every time. The lookup was the
untyped path's whole bareword chain (`push_bare_word_value`): import-alias scans, enum
probes with a `format!`, env probes, then the type registry. That is ~7,000 instructions for
an answer that never changes. JSON::Fast does it once per container and once per escaped
string. It was 10% of a whole SPDX decode, the largest single cost left (#9122).

A run-time cache was measured and rejected. The chain reads interpreter state spread over a
dozen fields, with no common generation to key on, and a key that misses one of them goes
stale only after a rare mutation. That is a flaky failure.

The name is instead settled where Raku settles it, at compile time
([ADR-0115](../../docs/adr/0115-core-type-names-in-nqp-operands-fold-at-parse-time.md)). At
the end of the parse, a CORE type name on a fixed list becomes an `Expr::Literal` of its type
object when it is a positional operand of an `nqp::` call, and only when the compunit binds
that name nowhere. The names that count as bound are:

- subs, types (including imported ones and a class declared later in the unit), enum values
  and imported functions;
- sigilless terms and parameters, and constants.

Every parser scope reports what it bound when it is popped. A unit whose imports the parse
cannot see in full folds nothing: one that imported through a `sub EXPORT` hook, or `use`d a
module no scan resolved. This is the rule the parser already applied to `Any` (#9047).

Two consumers had to learn that a literal type object is data:

- TRIR's `compile_literal` now admits it.
- The frame-lexical proof (ADR-0113) reads the serialized AST, and it rejected any `Package`
  key, which it meant as the package declaration statement. `Literal(Package(..))` shares
  that key. Without the fix, `unjsonify-string` lost its frame-lexical inner sub and fell out
  of TRIR, and the decode ran ten times slower.

Measured on a release build in a 4-core container:

- 727-record SPDX `from-json`: ~0.17 s → ~0.146 s (rakudo 0.044–0.061 s);
- `{}` element: ~5.0 → ~3.6 µs;
- escaped string element: ~12.5 → ~10.0 µs.

Pin: `t/vm/nqp-core-type-operand-fold.t`, with `t/lib/CoreNamedTypeExport.rakumod`,
checked against rakudo. It covers what folds and four bindings that must win over the CORE
name: a sigilless parameter, a nested sigilless `my`, a later class declaration, and an
imported type.
