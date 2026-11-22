# A precompiled module's BEGIN values are replayed instead of recomputed

A module served from the precompilation cache used to run every value-position
`BEGIN` of its mainline again on each load. `uniname-words` builds its word
table over every code point that way, so each warm `use` cost as much as the
first one.

Now the first load that runs such a mainline records, for each `BEGIN`
expression it can prove replay-safe, the value the body produced and the unit
lexicals it changed (ADR-18012 stage S1). The log is stored next to the compiled
entry as `{hash}.begin`, bound to that entry's checksum and the interpreter
version. A later load binds the recorded lexicals, pushes the recorded value and
skips the body.

A body is recorded only when it is replay-safe by construction; every other one
runs on each load, as before:

- every opcode is on a small allow-list, and every method and `nqp::` op it
  names is one whose built-in form only computes a value;
- no other compiled code runs while it runs. Every dispatch loop now takes its
  polls from `DispatchPolls::enter(code)`, which is the only constructor, so
  every loop reports the code it runs. A user method, operator candidate, regex
  code block or `where` clause reached from the body therefore marks it;
- the value and the changed lexicals decode back to exactly what they were,
  and they share no container.

The cache encoding now keeps a container's declared type, constraints, default
and nqp element kind. Before, a cached `Map` came back as a `Hash` and an
`int32` array as an untyped `Array`.

`MUTSU_BEGIN_REPLAY=0` turns recording and replay off.
`MUTSU_PRECOMP_TRACE=1` prints one line for each `BEGIN` that is recorded,
replayed or refused, with the reason it was refused.
