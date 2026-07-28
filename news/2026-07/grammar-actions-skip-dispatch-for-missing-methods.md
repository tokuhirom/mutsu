# Grammar action dispatch no longer pays full multi-dispatch resolution for a method that doesn't exist

`invoke_grammar_actions` walks a grammar's finished match tree bottom-up and,
for **every** named capture, unconditionally called
`call_method_with_values(actions, rule_name, ...)` to see whether the actions
class had a corresponding method — relying on the call failing with
`MethodNotFound` as the "no action for this rule" signal. Most match-tree
nodes are low-level helper tokens (YAMLish's `space`, `single-bare`,
`line-break`, `foldable-whitespace`, `comment`, ...) that an actions class
never defines a method for, so the overwhelmingly common case paid the full
cost of multi-dispatch resolution (`has_proto`, `has_multi_candidates`,
`has_multi_function`, `resolve_function_with_types`, `bare_name_packages`,
plus the `format!`-built error message) just to be told "not found".

A `perf` profile of `load-yaml` (the bundled YAMLish battery) over a
block-sequence document with several long, space-heavy quoted scalars showed
that dispatch-resolution cluster and its downstream allocation
(`malloc`/`cfree`/`format!`) as a large share of total samples; the total
sample count for the same input dropped by roughly half after this fix, with
the dispatch cluster itself almost disappearing from the profile.

The fix adds a cheap pre-check: `has_user_method` is a direct MRO walk + a
`HashMap`-by-name lookup (no candidate scanning, no error-message
formatting), so `invoke_grammar_actions` now calls `call_method_with_values`
only when the actions class (or its `:sym<...>` proto variant) actually
declares that method. The first attempt at this missed that a stateless
`:actions(Actions)` grammar action is commonly passed as the **bare type
object**, not an instance (`ValueView::Package`, not `ValueView::Instance`) —
without handling that case the class name was never resolved and the
pre-check silently fell back to "assume it exists, call anyway" for every
node, which is why the first perf comparison showed no change at all. Once
`ValueView::Package` was handled the same way `Instance` is, the dispatch
cluster collapsed as expected.

Pin: `t/grammar-actions-type-object-sparse.t` — a bare type-object actions
class with methods for only some rules (including a `:sym<>` proto variant),
verified against `raku` first (including its `Use of Nil in string context`
warning when a topmost `.made` reads through an un-actioned child).

Verified with `make test` (2552 files, 24408 tests, all green) and the
grammar/YAMLish-relevant roast whitelist files locally; the actual wall-clock
win will show up in the next `bench-data` run rather than a local
measurement, since this session's box was under heavy concurrent-build load
throughout.

See `todo/tickets/yaml-parse-throughput.md` for what's still open in the
broader YAML-parse-throughput investigation this came out of.
