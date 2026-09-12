# An aliased capture is one Match object, on both parse paths

A non-suppressing alias `<val=word>` files **one** capture under both names, so
`$m<val>` and `$m<word>` are the same `Match` — `===` on them is `True` in
rakudo. mutsu got that right only when an actions object was passed:

```
grammar A { token TOP { <val=word> }; token word { \w+ } }
my $m = A.parse('abc');
say $m<val> === $m<word>;                      # mutsu: False   raku: True
my $n = A.parse('abc', :actions(class { }));
say $n<val> === $n<word>;                      # mutsu: True    raku: True
```

The capture store was never the problem. #7576's round-10 fix already made such
an alias store a single `Arc<CapNode>` under both slots
(`build_named_candidates_from_inner`'s `shared_under_original`), and
`t/grammar/grammar-alias-action-fires-once.t` pins it — but that test parses
*with* `:actions(...)`, which is exactly the path that worked.

The identity was lost on the way **out** of the store. The lazy-Match
materialization built a child `Match` per *slot*: `named_slot_value` called
`Value::lazy_match` for every entry it walked, and each call mints a fresh
`MatchNode` with its own instance id. Two slots pointing at one node therefore
became two objects with two `.WHICH`es. The action-driven path builds its Match
objects through the reduce walk instead and kept the sharing, so the two paths
disagreed about whether an aliased capture is one object or two — a latent trap
for the engine's own invariants, not only for a program asking `===`.

## The fix

One memo per materialization, keyed by `Arc` identity, so a capture node that
two slots share yields the child `Match` built for it once. It covers both axes
(`.list` and `.hash`) because the positional and named loops of one
`materialize_map` now share the memo. `pos_slot_value` / `named_slot_value` keep
their old signatures as thin wrappers that pass a throwaway memo, so the regex
engine's mid-match variable binding is untouched.

The key is only ever compared while every `Arc` it came from is alive in the
parent's `CapChildren`, and the memo never outlives the materialization, so
there is no freed-address collision of the kind `WhichId` exists to prevent.

Pinned by `t/grammar/grammar-alias-capture-identity.t`: 15 assertions measured
against rakudo, covering the action-less and action-driven parses, `.WHICH` and
`eqv`, `.hash` as well as subscript reads, the alias inside a non-capturing
group / an alternation / a nested subrule, a quantified alias sharing per
iteration, a *suppressing* alias still filing one name only, and a plain
positional capture keeping a stable identity.

Closes [#8167](https://github.com/tokuhirom/mutsu/issues/8167).
