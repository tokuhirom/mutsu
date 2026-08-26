# `Pod::*` objects declare their attributes, so `.raku` renders them

`say $=pod[0].contents[0].contents.raku` printed `(Pod::FormattingCode.new,)`
where rakudo prints
`[Pod::FormattingCode.new(type => "C", meta => [], config => {}, contents => ["foo"])]`.
Every `Pod::*` block had the same symptom — the attribute list was missing
entirely.

## Root cause

Not a type-specific `.raku` gap: the generic instance `.raku`
(`Interpreter::default_instance_repr` → `collect_public_raku_attrs`)
enumerates the *declared* attributes of the value's class, and every `Pod::*`
class was registered in `runtime_init` with `attributes: Vec::new()`. The
attribute values were present in the instance's attribute map all along;
nothing was ever asked to print them.

## Fix

The ten Pod classes are registered from one table now, each with its real
attribute list in the order rakudo's `.raku` emits — the subclass's own
attributes first, then `config`, then `contents`:

| class | attributes |
|---|---|
| `Pod::Block` | `config`, `contents` |
| `Pod::Block::Code` / `::Comment` / `::Para` | `config`, `contents` |
| `Pod::Block::Named` | `name`, `config`, `contents` |
| `Pod::Block::Table` | `caption`, `headers`, `config`, `contents` |
| `Pod::Config` | `type`, `config` |
| `Pod::Defn` | `term`, `config`, `contents` |
| `Pod::FormattingCode` | `type`, `meta`, `config`, `contents` |
| `Pod::Heading` / `Pod::Item` | `level`, `config`, `contents` |

`collect_class_attributes` walks the MRO base-first and moves a redeclared
name to the end, so repeating `config`/`contents` after each subclass's own
attributes reproduces rakudo's order exactly. The `%`/`@` sigils keep the
rendered values un-itemized (`config => {}`, not `config => ${}`).

Two value-shape bugs the newly-visible output exposed were fixed with it:

- The outer-container mismatch the ticket noted (`(...)` vs `[...]`) was real:
  `contents`, `headers` and `meta` were built with `Value::array`, which makes
  an `ArrayKind::List`. They are `Value::real_array` now, matching rakudo's
  `@`-sigil attributes.
- `Pod::FormattingCode.meta` was a `Str` for `L<>` and absent for every other
  code. It is always a `Positional` now, matching rakudo: `[]` for a code with
  no metadata, `["target"]` for `L<display|target>`, `[["a","b"],["c"]]` for
  `X<display|a,b;c>` (semicolon-separated entries of comma-separated levels),
  and `[65]` / `["amp"]` for `E<65>` / `E<amp>`.

`.raku` output for every Pod block type now matches `raku` character for
character. `Pod::Block::Declarator` is registered too, declaring
`WHEREFORE`/`leading`/`trailing`/`config`/`contents` — it has to, because
registering a class makes its declared list the source of its accessors, and
`.WHY.leading` must keep working. Its `.raku` is not compared against rakudo's:
rakudo renders the documented routine inside it, complete with a per-run object
address, so there is no stable target to match.

Pinned by `t/pod-object-model.t`, which asserts the exact `.raku` strings and
passes under both `raku` and `mutsu`.
