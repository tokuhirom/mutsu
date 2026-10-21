# A lazy `.hyper` / `.race` keeps its configuration

The `HyperSeq` that `.hyper` / `.race` of an unbounded sequence answers now carries the
`:batch` and `:degree` it was given. `.configuration` reports them (or the defaults),
`.is-lazy` is `False` as in Rakudo, and a `.map` / `.grep` stage over it stays a
`HyperSeq` with the same configuration. `HyperConfiguration.raku` prints its fields
(`HyperConfiguration.new(batch => 4, degree => 2)`), for settled sequences too.

The answers are lazy pulls on `HyperSeq` / `RaceSeq` (`method_table/lazy_pull.rs`), whose
owner walk now knows those two reported types. Campaign issue #11276; fixes #14420.
