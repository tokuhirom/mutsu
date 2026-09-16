# Numbered captures through an optional quantified subgroup

`[ (x) +% ',' ]?` (and other list-quantified capture groups — `*`, `+`, `**`)
reserved a plain `Nil` positional slot when the whole optional group matched
zero times, instead of the empty list raku renders for a zero-iteration list
capture (`(x)*` on `""` is `$0 = []`, not `Nil`). `count_capture_groups`'s
stride only tracked how many slots to reserve, not which of them are
list-shaped, so `walk_zero_or_one_zero_arm` / `walk_seqalt_zero` always
reserved `Nil`.

This broke `@$0`/`@$1`-style arithmetic on the unmatched side of an optional
capture group — exactly the shape Net::Netmask's IPv6 grammar uses to parse
the `::` abbreviation:
`[ (<h16>) +% ':']? '::' [ (<h16>) +% ':' ]? <?{ @$0 + @$1 <= 8 }>` never
matched `'::1'` in mutsu, because the leading group's `$0` came back `Nil`
where raku's `[]` was expected.

Added `capture_group_list_flags`, mirroring `count_capture_groups`'s
traversal but recording, per reserved slot, whether an ancestor token is
itself list-quantified (`*`/`+`/`**`/a `%` separator) — the same rule
`collect_nested_list_quantified_names` already applies to named captures.
`CapStore::reserve_nil` and `reserve_nil_capture_slots` now take those flags
and push either a Nil slot or a zero-iteration list slot.

Also fixed a related bug found while testing this: a quantified *positional*
capture group rendered as a `List` (`Value::array`) instead of a true
`Array` (`Value::real_array`) — raku's `(x)+` gives `$0.raku` = `[...]`, but
mutsu gave `(...)`. Named quantified captures already used `real_array`;
this brings positional captures in line.

New regression test: `t/regex/match/regex-optional-group-list-positional-captures.t`.

Closes [#8585](https://github.com/tokuhirom/mutsu/issues/8585).
