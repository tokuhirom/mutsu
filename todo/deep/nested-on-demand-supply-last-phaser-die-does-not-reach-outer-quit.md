# A `die` in an on-demand supply's `LAST` phaser does not propagate as `QUIT` to an outer `whenever` subscribing to it

## TL;DR

When a `supply { whenever SOURCE -> $v { ...; LAST { die "reason" if ... } } }`
block is itself subscribed to by an OUTER `whenever` (in a `react {}` or
another `supply {}`), a `die` raised from the inner supply's `LAST` phaser
does not reach the outer subscription's `QUIT { ... }` handler in mutsu. In
`raku`, it does.

## Repro

```raku
my $s = Supplier.new;

my $inner = supply {
    my int $expected = 10;
    whenever $s.Supply -> $blob {
        emit $blob;
        $expected -= $blob.chars;
        LAST {
            if $expected != 0 {
                die "too short";
            }
        }
    }
};

start { sleep 0.05; $s.emit("ab"); $s.done; }

react {
    whenever $inner -> $v {
        say "GOT: $v";
        QUIT {
            say "QUIT: $_";
        }
    }
}
say "done";
```

- `raku`: prints `GOT: ab`, then `QUIT: too short` (the outer `whenever`'s
  `QUIT` phaser catches the inner supply's `LAST`-phaser die), and the
  program completes normally (`say "done"` still runs — a caught `QUIT`
  does not propagate further).
- `mutsu` (`583a45506` + in-flight `promise-supply-coercion-async-drive`
  branch — confirmed independent of that branch, see below): prints `GOT:
  ab`, then the exception propagates **uncaught** out of the `react` block
  ("A react block: ... Died because of the exception: too short"),
  crashing the process with a non-zero exit. `QUIT: too short` never
  prints; `say "done"` never runs.

## Discovery context

Found while verifying
`todo/tickets/promise-supply-coercion-drives-react-on-calling-thread.md`'s
fix (branch `promise-supply-coercion-async-drive`, moving the on-demand
`Promise(supply {...})` coercion's final react-loop drive to a background
thread so it no longer deadlocks the producer thread). That fix resolves the
ticket's PRIMARY symptom (the deadlock/hang) — verified via
`tmp/repro-promise-supply-coerce.raku` and `tmp/repro-bodytext-blocks.raku`,
both now matching `raku`'s completion behavior. But the ticket's third
verification repro, `tmp/repro-respparser-untilclosed.raku` (Case 2:
`Content-length: 1000` with a much shorter body, connection closed early —
uses `Cro::HTTP::RawBodyParser::ContentLength`, whose `parser` method is
exactly this shape: an inner `supply { whenever $raw-blobs {...} LAST { die
X::Cro::HTTP::RawBodyParser::ContentLength::TooShort.new if $expected != 0
} } }`, consumed by an outer `whenever` inside
`Cro::MessageWithBody`'s `body-blob = Promise(supply { whenever
self.body-byte-stream {...} })`), still gives the WRONG final state: the
body promise resolves `Kept` (with whatever partial bytes arrived) instead
of `Broken` with `X::Cro::HTTP::RawBodyParser::ContentLength::TooShort`.

The minimal repro above isolates that exact shape (nested on-demand
supply's `LAST`-phaser die reaching an outer subscriber's `QUIT`) with no
`Promise` coercion or Cro involved at all, and reproduces identically
**independent of the async-drive branch** (confirmed by running it against
plain `main` — same crash, same missing `QUIT:` line) — so this is a
separate, pre-existing bug in the react/on-demand-supply QUIT-propagation
mechanism, not something the async-drive change introduced or is
responsible for fixing. `promise-supply-coercion-drives-react-on-calling-thread.md`'s
own design decision (2026-08-10) explicitly scoped that fix to "option (a),
scoped to the final drive only" — this ticket is exactly the kind of
adjacent-but-separate gap that ticket's implementer was told to file rather
than fold in.

## Impact

- `t/http-response-parser.rakutest` subtest 120 ("Connection close with
  incomplete body throws") — the body promise needs to `Broken` with
  `X::Cro::HTTP::RawBodyParser::ContentLength::TooShort`; currently resolves
  `Kept` instead, silently swallowing the truncation.
- Subtest 111 ("Response with body terminated by close of connection", the
  simpler `RawBodyParser::UntilClosed` case with no `LAST`-phaser die) is
  UNAFFECTED by this gap and should now pass once the async-drive fix lands
  — confirmed via the ticket's Case-1-shaped repro
  (`tmp/repro-respparser-untilclosed.raku`'s first block), which matches
  `raku` exactly.
- Likely affects any Raku code relying on a `supply{}`-chain's `LAST`-phaser
  error signaling working through nested on-demand supplies generally, not
  just this one Cro path.

## Where to look

- The react drive loop's QUIT handling: `src/vm/vm_react_subscriptions.rs`
  (`drive_react_subscriptions_inner`) and however it dispatches an inner
  on-demand supply's failure/QUIT phasers today (`run_on_demand_body`'s
  handling of a body error in `src/runtime/supply_promise.rs`, and how that
  error is supposed to surface as a "quit" event on the outer subscription's
  channel rather than an uncaught Rust-level/RuntimeError bubbling straight
  out of the whole drive loop).
- Compare against how a `whenever` on a LIVE (Supplier-backed) source
  reports its own `die` as `quit` (that path clearly works elsewhere in the
  codebase — the difference here is the source is itself an ON-DEMAND
  supply chained one level deeper, not a raw Supplier).

## Verification (once fixed)

- The minimal repro above should print `GOT: ab` / `QUIT: too short` /
  `done`, matching `raku`.
- `tmp/repro-respparser-untilclosed.raku`'s Case 2 should print `C2 BODY
  status=Broken exception=X::Cro::HTTP::RawBodyParser::ContentLength::TooShort...`,
  matching `raku`.
- `t/http-response-parser.rakutest` subtest 120.
