# ADR-0115: A CORE type name used as an `nqp::` operand folds to its type object at parse time

- **Status**: Accepted (2026-09-23, direction chosen by tokuhirom: "resolve at compile time,
  write an ADR first"); implemented in the same PR as the decision.
- **Context**: [#9122](https://github.com/tokuhirom/mutsu/issues/9122), ADR-0112 Step 3,
  parent [#8673](https://github.com/tokuhirom/mutsu/issues/8673).

## 1. Problem

nqp-style code names CORE types as data:

```raku
my @result;
nqp::bindattr(@result, List, '$!reified', my $buffer := nqp::create(IterationBuffer));
my $output := nqp::create(Uni);
```

mutsu compiled each of those barewords to a run-time lookup: `GetBareWord` on the untyped
path, `TrOp::LoadBareWord` in TRIR. Both land in `push_bare_word_value`
(`src/vm/vm_var_get_ops.rs`), the untyped path's full bareword resolution chain:

- the `nqp::` prefix, `_`, and qualified-name visibility gates;
- the imported-routine alias scan, which is `bare_name_packages()` × `imported_routine_alias`;
- the smiley and parameterized-type rewrites;
- the enum probes, which include a `format!` of a package-qualified key;
- the env probes, then `running_module_bareword`, then the type registry.

A single `IterationBuffer` cost ~7,000 instructions. The answer is the same `Package(N)`
every time.

JSON::Fast names `List` / `Array` / `IterationBuffer` / `Map` / `Hash` once per container and
`Uni` / `NFD` once per escaped string. Across a 100-record SPDX decode, callgrind put
`push_bare_word_value` at **10% of the whole program's instructions**. It was the largest
single cost left after ADR-0112 Step 3's first two slices.

## 2. Options considered

1. **A run-time per-site cache.** Keep the resolved value per `LoadBareWord` site, validated
   by the registry write generation, `fn_resolve_gen`, the current package, the running
   frame's lexical package, and env absence of the name. Measured feasible: the registry
   generation was stable across the whole decode. Rejected because the chain reads more
   state than those keys cover. That state includes `suppressed_names`,
   `class_scoped_short_names`, the imported-routine alias table, the enum bare-name store and
   `module_scope_lexicals`. It is interpreter state spread over many fields with no common
   generation. A key that misses one of them gives an answer that is wrong only after some
   rare mutation, which is the flaky failure CLAUDE.md ranks as the worst risk.
2. **Resolve at compile time** (chosen). In Raku, a bareword type name is a lexical lookup
   settled at compile time. It means the CORE type unless something in the compunit binds
   the same name. A decision that depends only on the unit's source cannot go stale at run
   time.

## 3. Decision

At the end of `parser::parse_program`, the whole-program walk that already classifies `*`
leaves (`whatever_curry::mark`) also rewrites `Expr::BareWord(N)` to
`Expr::Literal(Value::package(N))` when all of the following hold:

- **Position:** the bareword is a positional operand of an `nqp::` call, in either
  `Expr::Call` or `Stmt::Call`. There the type object is plain data. No method call,
  smartmatch, coercion, trait or `when` reads it as a name, and those forms keep
  `Expr::BareWord` and their own compile paths, which dispatch on it.
- **Name:** `N` is on a fixed list of CORE types (`src/parser/core_type_fold.rs`). Each
  resolves to its own type object in a fresh interpreter, which was checked against rakudo
  (`::(N).^name eq N`).
- **Unit:** the compunit binds `N` nowhere. Every parser scope reports its bound names when
  it is popped, and every scope still open reports at the end. The names covered are:
  - user subs and imported functions;
  - user and imported types, including a `class` declared anywhere in the unit;
  - enum values, and imported value terms;
  - term symbols (sigilless `my \x`, sigilless parameters, `constant`);
  - compile-time constants.

  The check is unit-wide, not per scope: a name bound in any scope is folded nowhere in the
  unit.
- **Imports:** the unit's imports are fully visible to the parse. A unit that imported
  through a run-time `sub EXPORT` hook (`term_keywords_shadowable`) folds nothing. So does a
  unit that `use`d a module no scan resolved (`type_index_is_complete`, e.g. a `use lib`
  computed at run time).

This is the rule the parser already applies to the CORE term `Any`
(`term_literals::keyword_literal`, #9047), extended to a list of types in one position.

Both execution paths see the literal: the untyped compiler emits `LoadConst`, and TRIR
`ConstObj`. So ADR-0110's differential gate (TRIR on == TRIR off) is unaffected.

## 4. Consequences

- **Where mutsu differs from itself.** Before, the untyped path resolved these names
  dynamically, and a caller's env could leak into a module routine's lookup. Where that
  answered something other than the CORE type for a name the unit does not bind, the folded
  program now answers what Raku does. No test in `t/` or roast depended on the leak.
- **What it does not cover.** A name reached another way keeps its run-time lookup. That
  includes an operand of a non-`nqp::` routine, a method invocant (`List.new`), `::('List')`,
  and `CORE::List`. So do types off the list.
- **Two walkers learned that a literal type object is data.**
  - TRIR's `compile_literal` admits a `Package` value.
  - The frame-lexical proof (ADR-0113), which reads the serialized AST, no longer mistakes
    `Literal(Package(..))` for the `Package` declaration statement it rejects. It shares
    the JSON key. Without that fix, `unjsonify-string` lost its frame-lexical inner sub,
    and with it TRIR.

## 5. Measured

On a release build in a 4-core container:

| | before | after |
|---|---:|---:|
| 727-record SPDX `from-json` | ~0.17 s | ~0.146 s |
| `{}` element | ~5.0 µs | ~3.6 µs |
| escaped string element | ~12.5 µs | ~10.0 µs |

Rakudo measured 0.044–0.061 s on the same SPDX decode.

## 6. Pins

- `t/lang/core-type-fold-nqp-operand.t`, with `t/lib/CoreNamedTypeExport.rakumod`. It covers
  the folded operands, plus four names that must not fold: a sigilless parameter, a nested
  sigilless binding, a class declared later in the unit, and an imported type. All of it is
  checked against rakudo.
- The TRIR suites (`t/vm/codegen/adr0112-*.t`, `adr0110-trir-differential.t`) run with the
  fold in place.
