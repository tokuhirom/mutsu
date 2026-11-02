# `&put.wrap`, `&say.wrap`, `&print.wrap` and `&note.wrap` run their wrapper

`say`, `put`, `print` and `note` compile to their own opcodes, which never consulted the
wrap table, so `my $h = &put.wrap: { @out.push($_) }` captured nothing and the text went
straight to `$*OUT`. A wrapped output routine now runs its wrapper (the wrapper reaches the
real renderer with `callsame`), and `.restore` brings the plain behaviour back. Found with
Dawa, whose tests capture output this way: `t/03-commands.rakutest` goes from 0 to 5 of 5
passing, as on rakudo.
