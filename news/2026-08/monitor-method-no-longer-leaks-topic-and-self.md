# A monitor method call no longer overwrites the caller's `$_` and `self`

Calling *any* method on an `OO::Monitors` monitor reset the caller's topic to
`Any` and rebound the caller's `self` to a `MetamodelX::MonitorHOW`:

```raku
use OO::Monitors;
monitor M { method d($p) { 1 } }
my $m = M.new;
$_ = 'OUTER';
$m.d('x');
say $_;      # raku: OUTER    mutsu: (Any)
```

An empty method body was enough, so the leak was in the dispatch, not in
anything a monitor method did.

## Root cause

OO::Monitors wraps every method with `-> \SELF, | { … callsame }`, installed
from `MetamodelX::MonitorHOW.add_method`. After running the outermost wrapper,
the wrap-chain dispatch in `vm/vm_call_method_compiled.rs` copies the wrapper's
persisted closure-env overrides back into the caller's env for every name the
caller also has — so that a wrapper's writes to captured lexicals stay visible.

`$_` and `self` were swept up by that. Both are per-frame: each call gets its
own, and the caller's is restored from its saved env — the rule
`pop_caller_env_with_writeback` already applies to `$_`/`$/`/`$!` for dynamics.
`self` was the sharper failure, because the wrapper is installed from a method
of the *HOW*, so the value written back was the `MetamodelX::MonitorHOW`
itself; every subsequent `$!attr` read in the caller then threw `P6opaque: no
such attribute '$!x' on type … in a MetamodelX::MonitorHOW`.

A hand-written `.wrap` never showed either symptom, which is why this survived
the EXPORTHOW::DECLARE campaign.

## Fix

Skip `_`, `/`, `!` and `self` in that writeback. Ordinary captured-lexical
writes still propagate (pinned).

## Effect

`Cro::HTTP::Client` could not complete a single request. Its
`$!connection-cache` is a monitor, so `$!connection-cache.add-pipeline($pipeline)`
in the middle of the response `whenever` body destroyed the topic — and
`.request = $request-object` two lines later died with `X::Assignment::RO:
cannot assign through .request on non-instance`. Past that, the clobbered `self`
made `$!follow` unreadable.

A full client/server round trip now works:

```
run 1: status=200 body="Visit 1"
run 2: status=200 body="Visit 1"
run 3: status=200 body="Visit 1"
```

`t/http-auth-basic.rakutest` and `t/http-auth-basic-with-session.rakutest` no
longer time out (`rc=124` → they run and report real assertions), and
`t/http-session-inmemory.rakutest` gets from 1 test to 3. Neither file passes
yet — the response body still arrives empty in the session tests.

Pinned by `t/monitor-method-does-not-leak-topic-or-self.t`.

## Still open

A monitor method called by a *computed* name inside a `for` loop
(`for <d> -> $n { $_ = 'C'; $m."$n"('x') }`) still loses the topic; the static
call and the top-level computed call are both fixed. Recorded in
`todo/tickets/computed-monitor-method-call-in-a-loop-still-leaks-the-topic.md`.
