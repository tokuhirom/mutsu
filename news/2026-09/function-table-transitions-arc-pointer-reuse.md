# FunctionTableTransitions no longer aliases on Arc pointer reuse

`FunctionTableTransitions::install` (`src/runtime/function_table.rs`) memoizes
"installing this key with this definition, into the map named `V`, yields the
map named `W`" so that a routine-local `my sub`'s re-entry reuses the version
it produced last time instead of minting a fresh one on every call. The memo
key included the raw pointer value of the `Arc<FunctionDef>` being installed
as a stand-in for "this exact definition".

That is only sound while the allocation behind the pointer stays alive. Once
every `Arc` sharing an allocation is dropped, the allocator is free to hand
that exact address to a later, unrelated `Arc<FunctionDef>` — and the memo
then wrongly replayed the old target version for the new, different
definition. `runtime::function_table::tests::repeating_an_install_reuses_the_version_it_produced`
hit exactly this deterministically when run in isolation (its third step
installs a fresh, unrelated `FunctionDef` right after the earlier ones'
sole surviving `Arc` was dropped, so the allocator handed back the same
address and the memo returned a stale version).

The fix keeps a clone of each memoized `Arc<FunctionDef>` alongside its `seen`
entry. As long as an entry is live, its clone pins that allocation, so no
other `Arc<FunctionDef>` can ever be constructed at the same address in the
meantime — the address recorded in a live memo entry can only ever mean "a
clone of the very `Arc` recorded here". The memo cap (`TRANSITION_MEMO_CAP`)
already bounded how many entries — and now how many retained clones — can be
live at once, so this does not change the structure's memory-retention
characteristics beyond holding those definitions instead of dropping them
early.

Closes [#8934](https://github.com/tokuhirom/mutsu/issues/8934) and
[#8932](https://github.com/tokuhirom/mutsu/issues/8932) (duplicate reports of
the same bug).
