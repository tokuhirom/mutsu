# Regex-embedded `:our` declarator now writes through to package storage

A regex/token-embedded `:our $var = ...;` declarative-prefix modifier used to
behave exactly like `:my` in mutsu: it worked as a lexical binding for use
*within* the match (so `token TOP { :our $our = 'thor'; $our \s+ is \s+
mighty }` matched `'thor is mighty'` correctly), but the declared variable
never became visible through its package-qualified name afterward.

```raku
grammar HasOur {
    token TOP {
        :our $our = 'thor';
        $our \s+ is \s+ mighty
    }
}
say HasOur.parse('thor is mighty');
say $HasOur::our;
```

- raku: `｢thor is mighty｣` then `thor`
- mutsu (before): `｢thor is mighty｣` then `Nil`

## Root cause: two independent bugs, in two different code paths

Grammar `token`/`rule`/`regex` bodies compile their pattern into a structured
`RegexAtom` tree ONE TIME (not re-parsed from a raw string on every match), so
a declarative-prefix declarator inside a grammar token is handled by
`RegexAtom::VarDecl` in `src/runtime/regex/regex_match_capture.rs` — a
completely separate code path from the raw-string declarative-prefix handling
a plain `~~ / ... /` smartmatch uses (`src/runtime/regex/regex_match_public.rs`,
the mechanism `:my`/`:constant` caller-scope persistence was fixed in). The
grammar-token path only ever evaluated the declarator's RHS expression and
stashed the result in a per-match capture dict for in-match lexical use,
discarding the AST's `is_our` flag entirely — so the package-scoped
write-through never happened, regardless of `:my` vs `:our`.

A second, independent bug surfaced while fixing the first: even for a plain
(non-regex) `our $var = ...;` declared directly inside ANY sub, method, or
closure body, `Compiler::qualify_variable_name` unconditionally returned the
UNQUALIFIED bare name whenever the compile-time `current_package` held the
synthetic state-scope pseudo-package every routine body compiles with (e.g.
`Pkg::&foo/1`, assigned purely for `state`-variable key uniqueness). That made
`our $x = ...;` inside any routine silently write to no package at all instead
of the real enclosing one. Widening `qualify_variable_name` itself to fall
back to `Compiler::enclosing_package` (mirroring the existing
`Compiler::runtime_current_package` fallback) turned out to be too broad — most
of that function's *other* callers rely on the unqualified-fallback behavior as
a `GetGlobal` lookup for a free/captured lexical living in `env` under its bare
name, and qualifying those broke closure-captured-variable resolution across
~80 test files. The fix landed as a new, narrowly-scoped
`Compiler::qualify_our_variable_name`, used only at the three `our`-declaration
call sites in `compiler/stmt.rs` that are explicitly gated on `is_our`, leaving
the general bareword/closure-capture resolution path untouched.

With both fixed, `RegexAtom::VarDecl` now runs the WHOLE `Stmt::VarDecl`
statement (not just its RHS expression) through `eval_block_value` when
`is_our` is true, which exercises the same `OpCode::DeclareOurScalar` /
package-write machinery a plain `our $var = ...;` uses, and then reads the
resulting value back out of `env` for in-match lexical use exactly as before.

Regression test: `t/regex-our-declarator-writeback.t` (8 assertions, covering
the grammar-token case, a plain non-grammar token, a `:my` regression check
confirming it still stays regex-local, and the general non-regex `our`-in-sub
fix), cross-checked against real `raku`.
