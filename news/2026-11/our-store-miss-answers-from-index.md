# The `our` store answers a miss from its unqualified-name index

`get_our_var` is asked for keys the store does not hold far more often than for
keys it does: a term probe such as `\Real` (is there a sigil-less constant named
`Real`?) runs on type checks and walks the running package's `::` chain, asking
the store for `App::Moneymoor::Util::\Real`, `App::Moneymoor::\Real`, ... at
every level. Each miss went on to try the unmerged-store spelling (a string
build and an intern) and a lexical package's storage-name member (two interns,
a split and env probes): about 2,900 instructions.

Every key the store holds already puts its unqualified part (the sigil plus the
segment after the last `::`) into the append-only `our_var_unqualified` index,
and both redirects keep the key's unqualified part. A miss whose unqualified
part was never stored is now answered from that index, with one intern and one
set probe.

Callgrind, release build: a reduced App::Moneymoor
`t/19-period-property.rakutest` (one seed) went from 14.42G to 13.65G
instructions (-5.4%). Refs #17265.
