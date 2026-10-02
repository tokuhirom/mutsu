# `add_dispatchee` accepts anonymous and closure subs

`&proto.add_dispatchee: my sub (Str:D $s, |c) { ... }` used to die with
`Cannot add dispatchee: no routine named '' found`: multi candidates were
registry rows keyed by name, so a nameless sub could not be registered, and a
closure would have lost its captured lexicals anyway (#10929).

A registry row (`FunctionDef`) can now carry the code value it stands for
(`FunctionDef::dispatchee`). `add_dispatchee` with a code value files such a
row under the proto. Candidate selection ranks it by signature with the
declared candidates. Running it calls the value itself, inside the usual
samewith and multi-dispatch frames, so the candidate sees the lexicals it
closed over and `callsame`/`nextsame` reach the remaining candidates. A row
built from a value takes the value's identity into its fingerprint, so two
closures cloned from one literal stay two candidates. `.candidates`, `.cando`,
`nextcallee` and a proto taken as a code value hand back the value itself, and
`Routine.dispatchees` is answered like `.candidates`, counting the added
candidate as Rakudo does.

This unblocks `shorten-sub-commands` and `CLI::Version`, which the
`CLI::Ecosystem` dependency closure needs.
