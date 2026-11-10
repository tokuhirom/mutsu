# `.toggle` flattens its conditions lazily; `pull-one` over a lazy source is O(1)

`@list.toggle((* mod 10 < 5) xx *)` died with "Callable expected": `toggle`
took its iterable argument as a single test instead of flattening
`*@conditions`. It now draws tests from an iterable argument (an infinite
`xx *` Seq included) as the switch flips.

That exposed an O(n^2) in the `Iterator` of a lazy Seq: every `pull-one`
re-copied the whole materialised prefix. The new tail is now appended in
place, so a 20000-element `pull-one` loop dropped from 12 s to 0.3 s. ParaSeq's
`t/46-toggle.rakutest` passes.
