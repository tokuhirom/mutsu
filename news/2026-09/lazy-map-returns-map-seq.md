# A `.map` Seq returned from a lazy map pipeline is no longer rendered as empty

`(1..*).map({ (1..$_).map(* * 2) }).head(3)` used to print `(() () ())`
instead of rakudo's `((2) (2 4) (2 4 6))`, and `.raku` / `.flat` saw empty
elements too (#9619). The callback returns a not-yet-run `.map`/`.grep` `Seq`
(`SeqSource::MapGrep`, ADR-0058), and the lazy pipeline cached it un-reified,
so the renderers — which cannot run the VM — read its empty seed. Indexing
worked because it goes through the VM.

`reify_finite_pipe_value`, which the lazy pipeline already runs over each
callback result, now reifies such a Seq through `reify_map_grep_seq`. A
`MapGrep` source was materialized at the `.map` call, so it always ends; a
`.map` over an infinite source is a lazy pipe instead and stays lazy. The
reify marks the body retained, so rendering it does not count as consuming it.
