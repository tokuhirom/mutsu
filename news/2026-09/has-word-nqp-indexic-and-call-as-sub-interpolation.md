# has-word's basic test suite goes from `die` to green: `nqp::indexic`/`indexim`/`indexicim`, `.&sub()` string interpolation, and `nqp::push`'s return value

Working the `has-word` zef distribution (`ecosystem/dists/H/has-word~b2f64225.json`, status `red`)
surfaced three independent, general-purpose interpreter bugs — none specific to `has-word` itself.

## `nqp::indexic`/`nqp::indexim`/`nqp::indexicim` were entirely unimplemented

`has-word` builds its case/mark-insensitive word search on these three `nqp::` primitives, and
mutsu only had plain `nqp::index`/`nqp::rindex`. A haystack scan died immediately with
`Unsupported nqp:: op: nqp::indexic`. Added all three to `src/runtime/nqp_ops_str.rs`, matching the
codepoint-indexed, `-1`-on-absent contract `nqp::index` already has: `indexic` case-folds each
compared codepoint, `indexim` strips its NFD-decomposed combining mark, `indexicim` does both.
Verified against the `raku` oracle, including the case-insensitive-but-not-mark-insensitive
distinction (`café` vs `cafe` matches under `:i`, not under plain case-insensitive `indexic`) and
vice versa for `indexim`.

## `.&sub()` inside string interpolation called the sub with zero arguments

`has-word`'s test descriptions interpolate `"$needle.&mööse()"` — the call-as-sub postfix, invoked
during string interpolation the same way `"$needle.method()"` already was. The interpolation
parser's method-call-chain loop (`try_parse_interp_method_call`,
`src/parser/primary/string/interp_helpers.rs`) had no case for a `&`-prefixed segment: it broke out
of the dot-chain and left `.&mööse()` as unconsumed text, which a *different* interpolation rule
(the bareword `&func()` case) then re-scanned as a zero-argument call — silently dropping the
invocant. Added a dedicated branch that requires the parenthesized form (matching every other
`.method` case here, and matching how rakudo itself leaves a paren-less `.&sub` as literal text in a
string) and builds the same `DynamicMethodCall` node the non-interpolated parser already produces
for `.&sub(...)`, including flushing any paren-less `.method` segments collected earlier in the
chain (`"$needle.uc.&mööse()"`).

## `nqp::push`/`push_i`/`push_s`/`push_n` returned the array instead of the pushed value

`has-word`'s `find-all-words` chains straight off the return value:
`nqp::add_i(nqp::push_i(@positions,$pos),$move)`. Verified against `raku`: all four `push` variants
(typed and untyped) hand back the *value just pushed*, not the array — a common nqp idiom for
building a running index without a separate temporary. mutsu's `push_elem`
(`src/runtime/nqp_ops_text.rs`) and the untyped `"push"` arm (`src/runtime/nqp_ops_list.rs`) both
returned `target.clone()` (the array) instead, so `find-all-words` accumulated `$move` (a constant)
into `$pos` on every iteration after the first instead of the real next position, silently
duplicating the last found index (`[0, 8, 8, 8, 8]` instead of `[0, 8]`).

## Result

`t/01-basic.rakutest` (has-word's main suite) goes from dying at the first assertion (`nok=0,
ok=1/30`) to a clean 30/30 pass. `t/02-selective-importing.rakutest` still has a residual gap —
symbols a custom `sub EXPORT` installs are invisible to `MY::`/pseudo-stash lookups even though they
resolve correctly by bareword call — filed as
[#8564](https://github.com/tokuhirom/mutsu/issues/8564) since it traces into the `Env` scoped-overlay
/ parent-chain model `CLAUDE.md` calls out as the highest-risk "env_dirty dual store" territory, not
a bounded local fix.

Regression tests: `t/vm/nqp-text-unicode-ops.t` (the three new `nqp::` ops),
`t/vm/nqp-list-hash-ops.t` (`push`'s return value), `t/lang/quoting/interp-call-as-sub.t`
(`.&sub()` interpolation, new file).
