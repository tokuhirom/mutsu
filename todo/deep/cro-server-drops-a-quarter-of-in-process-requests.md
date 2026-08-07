# Cro's server drops ~25% of requests when a Cro client shares its process

A Cro HTTP server and a Cro HTTP client running in **one mutsu process** lose
roughly a quarter of the round trips: the client writes the request, the server
reads the bytes off the socket, and the request then vanishes between
`Cro::TCP::ServerConnection.incoming` and `Cro::HTTP::RequestParser`'s
`whenever`. The client's response promise is eventually kept with `Nil` by the
react drive loop's 30-second deadline, so `await`ing it answers `Any`.

This is the dominant blocker in Cro's `t/http-middleware.rakutest` today: with
the loop-body-`state` reset repaired (2026-08-07, PR #6023) the file runs to
completion instead of blowing the stack, but which of its 11 subtests pass is
now a coin flip — observed 1, 3, 6 and 9 failures across four consecutive runs
of the same binary. Every failure is this same lost round trip, and each one
also costs 30 seconds, which is what pushes the file over its time budget.

It supersedes
`todo/tickets/cro-middleware-await-body-text-dies-coercing-any-into-promise.md`,
which recorded one face of it: `is await($resp.body-text), '1'` reported
"Impossible coercion from 'Any' into 'Promise'" because `$resp` was the `Any`
this lost round trip leaves behind, so `$resp.body-text` was `Any` too and
`await` had nothing to coerce. That ticket read the failure as an early-response
body that was never attached; it is really the generic dropped request, and it
is not specific to subtest 4 (it was simply the subtest that lost the coin flip
that day).

## It is neither the client nor the server alone

Three configurations, same mutsu build (`target/debug`), same route:

| configuration | result |
| --- | --- |
| Cro server in mutsu, `curl` × 30 from the shell | 30/30 OK |
| Cro server in one mutsu process, `Cro::HTTP::Client` × 30 in another | 30/30 OK |
| Cro server **and** `Cro::HTTP::Client` in one mutsu process, × 20 | 4-6 lost |

Restarting the server per request, keeping one server for all requests, and
using a fresh port per round all show the same rate, so it is not port reuse or
listener teardown.

## Where the request is lost

Instrumenting each layer of the two pipelines with `note` (client → server) and
running 20 sequential requests, a **bad** round prints exactly:

```
DBGPARSE transformer tapped          # RequestParser's supply is already tapped
DBGTCP   writing 88 bytes            # client's Cro::TCP::Connector::Transform
DBGTCP   write completed             # the socket write itself succeeded
DBGSRVCONN read 88 bytes             # ServerConnection.incoming got the bytes
DBGCLI   --- request end Any         # ...30s later
```

and a **good** round prints the same five lines plus

```
DBGPARSE packet 88 bytes expecting=RequestLine
DBGPARSE got request line GET /index.shtml
DBGSRV   handler ran
```

So the byte path is intact all the way into
`Cro::TCP::ServerConnection.incoming`:

```raku
method incoming() {
    supply {
        whenever $!socket.Supply(:bin) -> $data {
            emit Cro::TCP::Message.new(:$data, connection => $!socket);
        }
    }
}
```

The `emit` runs, the downstream `whenever $in` in
`Cro::HTTP::RequestParser.transformer` is already tapped (its "transformer
tapped" note precedes the read), and the message still never arrives. **The
loss is inside mutsu's supply plumbing between an on-demand supply's `emit` and
the tap of the supply composed onto it** — not in sockets, not in Cro.

## What has been ruled out

- **`Promise(supply { … })` giving up early.** It does not: the drive loop exits
  through the 30-second `deadline` branch in
  `drive_react_subscriptions_loop` (`src/vm/vm_react_subscriptions.rs`), i.e.
  the supply legitimately never completes because the response never comes.
  The immediate-looking failure is the 30s wait, not a premature keep.
- **The pipeline dying.** `Cro::HTTP::Client`'s `Pipeline` `LAST`/`QUIT`
  handlers never fire on a bad round, and `send-request` hands back a `Planned`
  promise as usual.
- **`Cro.compose` mis-splicing.** Fixed separately (PR #6023); the composed
  component list is correct on every round now.

## Not yet reproduced without Cro

A two-stage chain over an accepted socket in one process —
`supply { whenever $sock.Supply(:bin) { emit } }` fed into
`supply { whenever $stage1 { emit } }`, client and server both in-process — is
clean 20/20 under mutsu. The real server chain has more between the two ends
(`Cro::ConnectionManager`, the composed transform list, `Cro::ConnectionState`),
so the next step is to shadow-bisect the real pipeline: keep Cro's own classes
and cut components out of the composed list until the drop stops.

## Candidate to re-examine while there

`drive_react_subscriptions_loop` concludes "every `whenever` finished" from
`react_subs` alone, without first draining `self.pending_react_subscriptions` —
the queue holding `whenever`s a callback registered during the round just run,
which are only adopted at the top of the *next* iteration. Adding a
`continue 'react_loop` when that queue is non-empty is a small, plausible
correctness fix, but it did **not** change this failure rate (the loop here
exits via the deadline, never via the completion check), so it needs its own
repro before it is worth landing.

## How to reproduce

`tmp/` is scratch, so recreate these:

```raku
# tmp/cro-body-flake.p6 — one server, N client requests, all in-process
use Cro::Service; use Cro::HTTP::Server; use Cro::HTTP::Client;
use Cro::HTTP::Router; use Cro::HTTP::Response;

my $application = route { get -> 'index.shtml' { content 'text/html', 'Correct Answer' } };
my Cro::Service $service = Cro::HTTP::Server.new(
    :host('localhost'), :port(31415), application => $application);
$service.start;
my $bad = 0;
for 1..20 {
    my $resp = await Cro::HTTP::Client.get('http://localhost:31415/index.shtml');
    $bad++ unless $resp ~~ Cro::HTTP::Response;
}
say "bad: $bad / 20";
$service.stop();
```

Run it with the vendored Cro include paths (`tmp/cro-work/inc-paths.txt` plus
the Cro::HTTP dist's `lib`), from the dist directory. Expect 2-6 bad rounds and
a wall clock of 30s per bad round.
