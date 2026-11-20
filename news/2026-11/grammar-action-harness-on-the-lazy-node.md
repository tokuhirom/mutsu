# A grammar action runs on its lazy Match node, without the env snapshot

A grammar action costs a method call plus the "harness" around it, which
emulates dynamic scope. Before this change the harness cost ~52k
instructions per action against ~0 on rakudo (#18207). For every reduced
node with an action method, it:

- snapshotted the whole visible env into a SipHash `HashMap` and diffed it
  afterwards, to find the caller lexicals the action rebound;
- materialized the lazy Match into an `Instance` attribute map (~16k
  instructions) and dropped it again (~10k), even when the action never
  looked at `$/`;
- installed `$/`, `$_`, `$<name>` and `$0`.. under `String` keys, and put back
  the parent's afterwards;
- scanned the whole env after the call for a mutated copy of the actions
  object.

Now:

- The caller lexicals an action rebinds come from the carrier write log that
  every by-name writer already feeds; no snapshot is taken.
- The walk reads a node's children straight from its capture node. It builds
  a Match value only for a child whose action has not run yet. A child that
  already reduced is skipped, and so is a positional `( )` group that is a
  bare leaf: a group is not a rule and has no action.
- An action method that binds its own `$/` (`method foo($/) { ... }`) reads
  its captures through that parameter, in its body and in every closure inside
  it, and has its own `$_`. So the env ceremony around it is unobservable and
  is skipped. Its method resolution is reused for the call. Any other action
  (a signature without `$/`, a `FALLBACK`, a wrapped or multi method) keeps
  the ceremony, now keyed by pre-interned symbols. The ceremony also puts back
  the caller's `$/`.
- The actions object is mutated in place, so the env scan for a copy of it is
  gone. So is the second write of the `actions` attribute onto the parse
  result.

On #18207's repro (one trivial `method pchars($/) { make 1 }` per parse), a
profiling build measures, as the callgrind slope between 2000 and 4000 parses:

| | before | after |
| --- | ---: | ---: |
| `G.parse('/page', :actions(A0))` | 168.5k | 127.2k |
| `G.parse('/page')` | 95.6k | 95.1k |
| harness: (with − without) − the action method call | ~52k | ~12.8k |

Of the remaining ~12.8k, ~5.7k is outside the parse. It is the generic cost
of a method call that passes a named argument; #18282 is part of it.
