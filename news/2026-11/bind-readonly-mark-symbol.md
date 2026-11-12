# A `:=` bind no longer builds and interns its variable's name

A `:=` bind of a `$` variable to a containerless value is marked read-only
when it runs: `my $cmp := cmp($a, $b)`, which Array::Sorted::Util's binary
search does on every iteration, or `$c := 1`. Each time, the mark allocated a
`String` holding the variable's bare name and interned it again. A `$` local
is already stored under its bare name, and the chunk already holds that
name's symbol, so the mark now uses the symbol directly. That saves about
1.6k instructions per bind on a debug build (#15786).
