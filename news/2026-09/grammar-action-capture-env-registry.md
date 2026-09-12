# A grammar action call stops scanning the whole env for its captures

An action method sees `$<name>` and `$0..` for *its own* match, so every action
invocation hides whatever the parent action left under those names, installs
this match's, and puts the parent's back. That ceremony was written out inline
at both call sites — `invoke_grammar_actions` and its childless-leaf fast path
`invoke_leaf_action_lazy` — as an `O(env)` scan filtering `k.starts_with("<") &&
k.ends_with(">")`, plus `for i in 0..10 { env.remove(&i.to_string()) }`.

Both spellings ask a question the symbol table already answers. Filtering the
visible env resolves **every** key to a `&'static str` (a thread-local round
trip) and scans its bytes twice; the digit loop allocates ten `String`s and
interns ten symbols. Measured on a 60-row YAML document under callgrind: about
**5,300 instructions per scan with two scans per action**, and **25 interns per
action** — together **1.80% of the whole program**, which is what removing it
bought (1,559,607,721 -> 1,531,462,938 Ir).

The capture-shape registry (`symbol::capture_shaped_symbols`) is a superset of
the capture keys any env can hold — a key must be interned before it can *be* a
key — so walking it and probing the overlay is `O(capture names)`, a handful,
with no allocation and no string scan. `Interpreter::reset_capture_env_vars`
already ran on exactly that reasoning; this puts the action path on it too, in
one shared helper (`runtime/methods_grammar_action_env.rs`) instead of two
inline copies, with `$0..$9` served from a symbol table interned once per
process.

The probes are overlay-only (`Env::overlay_get_sym`), matching the `env.iter()`
/ `env.keys()` scans they replace exactly: a binding inherited from an enclosing
call frame is not this action's to hide. The restore re-reads the registry
rather than reusing the save's snapshot, because an action may intern a capture
name that did not exist when the save ran — which the scan saw too.

Pinned by `t/grammar/grammar-action-capture-env-isolation.t`, verified against
rakudo 2026.07: a leaf action sees its own `$<tag>`/`$0`, and the parent
action's are back when it resumes.

**Method note.** This item was not on the ticket's carried-forward list. Round
17 had measured `invoke_leaf_action_lazy` at "~0.1%" from its *self* cost; its
**inclusive** cost was 9.58%, about 100,000 instructions per call for 1,500
calls, and the env ceremony was a fifth of that. Reading a self-cost column and
a caller-attribution tree are different measurements, and only the second one
sees work a function does through its callees.

Refs [#7576](https://github.com/tokuhirom/mutsu/issues/7576).
