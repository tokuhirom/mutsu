# A Slip argument slips into `**@` and into `unique`/`squish`/`repeated`

A non-flattening `**@` slurpy keeps each iterable argument as one element, but
a `Slip` is exactly the value that asks to be flattened into its surroundings:
`sub g(**@a) { @a }; g((1, 2).Slip, 3)` now binds `[1, 2, 3]` instead of
`[slip(1, 2), 3]`. The routine forms of `unique`, `repeated` and `squish`
(`+values`) slip their Slip arguments the same way, so
`unique(self.keys.Slip, %!required.keys.sort.Slip)` (PDF::COS::Tie::Hash's
`check`) answers the keys rather than two lists. The three routines now share
one argument-gathering helper.
