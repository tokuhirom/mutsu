# `raku` of a typed container is the `raku` row's

The three by-name gates that rendered `Hash[Int,Str].new(...)`, `Array[Int].new(...)` and
`array[int].new(:shape(...), ...)` over the pure answer are one routine now, called by the
`Array`/`Hash` `raku` rows themselves (ADR-11276 §9.185). A shaped or itemized container reaches
those rows through the shapeless entry; answers are unchanged.
