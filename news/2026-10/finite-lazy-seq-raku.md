# A finite `.lazy` Seq keeps its elements, its laziness and its `.raku`

`(5,).lazy.raku` printed `().lazy.Seq` and `(1..5).lazy.map(* + 1).raku`
printed a plain `(2, 3, 4, 5, 6).Seq`. `(1..*).lazy` was still the Range.
Rakudo prints `(5).lazy.Seq` and `(2, 3, 4, 5, 6).lazy.Seq`, and `(1..*).lazy`
is a Seq with a 100-element `.raku` prefix (#10918). There were four separate
causes:

- A bounded pull of a list that has no generator, only a cache (`.lazy` over a
  finite list), ran the list's empty body through the prefix bridge and got
  nothing back. The pull now reads such a list's cache directly.
  `LazyList::is_cache_only` also now excludes a lazy `WALK` list, which does
  have a generator.
- `.map`/`.grep` over an explicitly `.lazy` finite list forced it and mapped
  eagerly. It now appends a lazy pipe stage, as for any other lazy source. The
  stage inherits the `lazy` marker, so it is `.is-lazy` and runs its callback
  only as elements are pulled. The three call sites that decide this share one
  predicate, `LazyList::map_grep_appends_stage`.
- `.lazy` on an unbounded range returned the range unchanged. It now wraps the
  range in an identity index pipe (`IndexTransform::Identity`).
- A `$`-held lazy Seq now renders itemized: `$((1, 2, 3).lazy.Seq)`.
