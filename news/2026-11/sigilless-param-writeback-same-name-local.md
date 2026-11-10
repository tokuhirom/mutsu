# A sigilless parameter's frame-exit write-back no longer clobbers a same-named local

When a routine took a sigilless parameter (`\object`) bound to a caller variable
and declared its own lexical with the caller variable's name (`my \value = ...`),
the parameter's frame-exit flush wrote through the alias chain into the
callee's binding, which could be a hash element cell: the hash then contained
itself (`{:a(%Hash)}`) after the call. The flush now leaves a callee-declared
name alone; the caller's variable is still updated by the post-merge re-apply.

Separately, `Hash.raku` / `Pair.raku` print a key made of a letter plus
combining marks in the adverbial `:key(value)` form, as rakudo does.

Found by running the LLM::Chat distribution (`t/29-wire-json.rakutest`).
