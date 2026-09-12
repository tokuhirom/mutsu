# Two matcher hot paths ask their map once instead of twice

Two places in the regex engine hashed and probed the same key twice in a row to
answer one question.

**Merging a capture delta.** `CapStore::merge_delta` pushed an undo record for
each named key and then took an `entry` on the same map to append the delta's
nodes. The record wants exactly the slot state the `entry` is about to hand out,
so taking the entry first and reading the record off it is the same work minus
one hash and one search per merged capture — about 57,000 of them on a 60-row
YAML document.

The same function also merged three maps that live in the accumulator's cold
payload (`RareCaps`): capture aliases, hash captures and `:my` lexicals. A delta
that never wrote one — the overwhelming majority — still paid three `take_*`
calls, each of which built an empty map, iterated it, and pruned an absent
payload. One `delta.has_rare()` branch answers all three at once. Together:
`merge_delta` **93,006,964 -> 82,107,307 Ir (-0.81% of the program)**.

**Starting a left-recursion activation.** Every `<subrule>` call asked
`lr_key_is_active` and then, on either answer, took an `entry` on the same
thread-local map — either to read the seed (a left-recursive re-entry) or to
begin the activation. `lr_begin_or_reenter` does both in one map operation and
returns which happened. The two spellings were otherwise identical: the separate
`lr_key_is_active` declined to create a vacant entry, but the
`lr_begin_activation` that always followed it created one anyway. That removes
87,211 probes on the same document, none of whose rules are left-recursive at
all.

**What is still there.** The left-recursion bookkeeping remains the largest
identified avoidable cluster in the profile: 224,644 map `entry` operations for
112,322 activations, plus the seed-consulted reads, at roughly 2% of the run on
a grammar with no left recursion anywhere. Removing it wants a gate on the
call-graph reachability analysis `regex_call_graph::reenter_decline` already
computes — which is not a slice, because the LR key deliberately carries no
package and the analysis is per-package, so skipping an activation changes what
a same-named rule in another grammar sees. It is recorded on the ticket rather
than guessed at here.

Refs [#7576](https://github.com/tokuhirom/mutsu/issues/7576).
