# A closure can rebind a captured scalar more than once

`$s := 7; $s := 17` inside a closure left the captured outer `$s` at 7. The
first bind seated a binding cell in the closure's env entry, but the
cross-thread mirror (`set_shared_var`) at the end of the same store then wrote
the bare value over that entry, so the second bind found no cell to reseat. The
store now restores the cell after the mirror. This fixes IRC::Log's
`search(:reverse, ...)`, and with it IRC::Log::Colabti's `t/02-search.rakutest`.
