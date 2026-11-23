# A `BEGIN` over the unit's variables runs when the parser reaches it

`my @log; BEGIN @log.push(1);` and `my $n = 0; BEGIN $n++;` used to keep their
`BEGIN` in the BEGIN-time prologue, and every `use` after them loaded there
too. They now run at parse time, as on rakudo (ADR-17558 §7.17).

The `BEGIN` runs over parse-time copies of the unit's plain `my` variables: a
copy starts without the initializer, as rakudo's `BEGIN` sees it. The values
it leaves are written back into the unit's variables at the `BEGIN`'s place
in the prologue. So `BEGIN $n++; BEGIN say $n;` prints `1` before a later
syntax error, and `say $n` in the mainline still sees what the `BEGIN`s did.
