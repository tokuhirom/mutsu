# A double-quoted regex atom interpolates subscripts, method calls and blocks

A `"..."` atom inside a regex follows qq-string rules. Rakudo therefore matches
all of these, but mutsu matched none of them
([#9628](https://github.com/tokuhirom/mutsu/issues/9628)):

```raku
"x p"   ~~ /"x @a[0]"/;
"x p,q" ~~ /"x @a.join(",")"/;
"x p"   ~~ /"x %h<a>"/;
"x 3"   ~~ /"x {1+2}"/;
```

The atom used to be interpolated at match time by the text pre-pass
(`interpolate_regex_scalars`). The pre-pass can resolve a bare `$name` from `env`.
It could not evaluate a subscript, a method call or a block without re-parsing
source at run time, which is the tree-walk slow path AGENTS.md bans.

Such an atom is now lowered at compile time instead. `Compiler::compile_regex_qq_thunks`
(`src/compiler/regex_qq_thunks.rs`) passes the atom's body to the one qq-string
interpolation parser (`parse_dispatch::parse_qq_interpolation`). It then compiles the
body as an ordinary block closure in the literal's defining scope. The closure is
captured on the `RegexClosure` scope under a new `MetaNs::RegexQq` key, derived from
the body text.

At match time the pieces work like this:
- `install_env_scope` runs the thunk once per match.
- The pre-pass (`splice_regex_qq_thunk_result`) splices the thunk's string result in
  as a single-quoted literal. The atom therefore still honors `:i` and quantifiers,
  and metacharacters in the result stay literal.
- Nested installs of the same regex, which happen when both the VM's smartmatch op and
  `smart_match` install the scope, reuse the active result. A side-effecting atom
  therefore runs once per match, as in Rakudo.

The compiler and the pre-pass find atoms with the same scanner (`src/regex_qq_atoms.rs`),
so they agree on the key. The scanner treats a `"` inside a `.method(...)` argument list
or an embedded `{ }` block as code, not as the closing quote.

Bodies that read match state (`$/`, `$0`, `$<x>`, `$_`) are left on the old path, and so
are patterns that declare `:my` lexicals.

Two related fixes were needed along the way:
- The VM's native `.subst` fast path never installed a regex closure's scope. A stored
  regex that closed over its defining scope therefore lost its lexicals under
  `.subst(...)`. It now installs the scope, as `~~` does.
- A pattern whose only interpolation is a `"..."` block with no sigil (`"x {1+2}"`) is
  no longer classed as static by `regex_pattern_is_static`. Before, its parse was
  cached without the per-match result.

`s///`, `token`/`rule` bodies and `<$re>`-interpolated regexes do not reach the thunk
yet. They are tracked in [#9673](https://github.com/tokuhirom/mutsu/issues/9673).
