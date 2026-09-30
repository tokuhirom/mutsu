# Multi-parameter `for` no longer overwrites the outer `$_`

`for LIST -> $a, $b { ... }` used to set `$_` to the per-iteration batch list,
so code such as `given $node { for %rename.kv -> $old, $new { $_{$old} ... } }`
indexed the batch instead of `$node`. The loop now hands each batch to its
parameter binds through a hidden variable (`wk::for_chunk`), leaving `$_` as the
enclosing topic, as Rakudo does. `-> $_, $n` still binds `$_` to the first
element. Closes #9978 (LLM::Graph `t/04-spec-synonyms.rakutest`).
