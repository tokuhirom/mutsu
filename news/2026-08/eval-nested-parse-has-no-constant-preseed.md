# An `EVAL`'d snippet's parse now sees the outer unit's constants

Split out of PR #6840, which taught the parse-time index about imported
constants but knowingly left one narrowing open: an `EVAL`'d snippet's nested
parse got a **type** preseed via `collect_eval_user_type_names`, but no
**constant** preseed. Since `reset_user_subs` starts the nested parse from an
empty scope stack, every constant the calling unit declared looked undeclared to
it, and the `when`-matcher gobbled-block diagnostic fired on valid code:

```raku
constant Foo = 1;
EVAL 'given 1 { when Foo { say "matched" } }';
```

rakudo prints `matched`; mutsu died at compile time with
`Function 'Foo' needs parens to avoid gobbling block`.

## Root cause

`bareword_names_known_term` (`src/parser/stmt/control/given_when.rs`) asks a
family of parse-scope registries whether a bareword `when` matcher names
something already declared. A `constant Foo = 1` and a sigilless `my \Bar = 2`
both register a `TermBinding::Value` term symbol, which
`is_user_declared_value_term` reads — but that registry lives in the parser's
scope stack, and `reset_user_subs` wipes the stack at the start of every
compilation unit. The EVAL entry point re-registered operators, imported
functions, user subs and user *types* afterwards; value terms were simply
missing from that list.

## The fix

Added the constant-term twin of the existing type preseed, following exactly the
same plumbing so the two cannot drift:

- `EVAL_USER_VALUE_TERM_PRESEED` thread-local + `set_eval_user_value_term_preseed`
  (`src/parser/stmt/simple.rs`, `simple/pragma_preseed.rs`), applied in
  `reset_user_subs` via `register_user_term_symbol`.
- `parse_program_with_operators_and_user_subs` takes and clears it alongside the
  other preseeds.
- `collect_eval_user_value_term_names` (`src/runtime/system_eval_string.rs`)
  supplies it. Constants have no runtime registry the way classes/roles/enums
  do, but every declaration leaves a `__mutsu_constant_var::<name>` marker in the
  environment — that marker set *is* the term-symbol set the parser wants back.
  Sigiled constants (`constant $x = 1`) are filtered out: they are read through
  their sigil and are never bareword terms.

The diagnostic still fires for genuinely undeclared names
(`EVAL 'given 1 { when X::Nope { } }'` is still a compile-time error), so this
narrows the false-positive without weakening the check.

Pin: `t/eval-compunit-introspection.t` covers both a `constant` and a sigilless
`my \term`, and passes verbatim under `raku`.
