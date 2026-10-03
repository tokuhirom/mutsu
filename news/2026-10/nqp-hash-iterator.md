# `nqp::iterator`, `nqp::iterkey_s` and `nqp::iterval`

Part of the `nqp::` coverage campaign (#11488), this resolves the hash ops
tracked by #11494.

`nqp::iterator` is the loop Rakudo's own `Map`/`Hash` internals and nqp-level
serializers are built on:

```raku
my $it := nqp::iterator(nqp::getattr(%h, Map, '$!storage'));
while $it { my $e := nqp::shift($it); f(nqp::iterkey_s($e), nqp::iterval($e)) }
```

It used to die with `Unsupported nqp:: op`. Now it returns a `BOOTIter`
object that is truthy while elements remain:

- On a list, `nqp::shift` answers the next element.
- On a hash, `nqp::shift` answers the iterator itself, positioned on the next
  pair, and `iterkey_s` / `iterval` read that pair's key and value.
- Reading before the first `shift`, or shifting past the end, dies with
  MoarVM's message.
- A hash iterator iterates over a snapshot of the hash, taken in the order
  `.keys` reports. So mutating the hash during the loop cannot invalidate it.

The truthiness lives in `Value::truthy`, so `while $it` and `nqp::istrue($it)`
need no special case. `shift` goes through `Interpreter::nqp_shift`, the body
that TRIR shares, so a hot loop compiled by TRIR takes the same path.

The other seven ops in #11494, `atkey_i`/`_n`/`_s`/`_u` and
`bindkey_i`/`_n`/`_s`, are recorded as not applicable rather than
implemented. Every Raku call to them dies in Rakudo:

- MoarVM's `VMHash`, which `nqp::hash` and a Hash's `$!storage` both are,
  dies with "does not support native type storage".
- A `CStruct` "does not support associative access".

`docs/nqp-op-coverage.md` lists them, with that evidence, under
"Not applicable".
