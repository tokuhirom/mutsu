# `cas($!attr, …)` loses updates in a debug build

`roast/S17-lowlevel/cas.t` (tests 21-24, "CAS on linked list with Scalar attribute
head works") and `roast/S17-lowlevel/cas-int.t` (tests 9-12) fail **every** run
against a **debug** binary, and pass every run against a **release** one. Both are
whitelisted and `make roast` (release) is green, which is why this has gone
unnoticed: the race window is simply wide enough in debug to hit reliably.

This is not a regression from any in-flight branch — verified by stashing,
rebuilding a clean `main` in debug, and reproducing 3/3.

## Repro

```sh
cargo build
MUTSU_BIN=target/debug/mutsu MUTSU_FUDGE=1 \
  prove -e 'scripts/run-roast-test.sh' roast/S17-lowlevel/cas.t      # 5/5 FAIL
MUTSU_BIN=target/release/mutsu MUTSU_FUDGE=1 \
  prove -e 'scripts/run-roast-test.sh' roast/S17-lowlevel/cas.t      # 3/3 PASS
```

## Symptom

The test builds a linked list from four threads, each pushing 1000 nodes with a
compare-and-swap retry loop, then sums the list:

```raku
my class NodeHead {
    has $.head = Node;
    method add-a-thousand-nodes() {
        for 1..1000 -> $i {
            loop {
                my $orig = $!head;
                my $next = Node.new(value => $i, next => $orig);
                last if cas($!head, $orig, $next) === $orig;
            }
        }
    }
}
my $head-obj = NodeHead.new;
await start { $head-obj.add-a-thousand-nodes() } xx 4;
```

Expected total `2002000`; observed `1527817`, `1469369`, `1493850`, `1555856` —
roughly a quarter of the nodes lost, the signature of a CAS that succeeds against
a stale view instead of the live one.

## Where to look

`cas` on an attribute is an `is rw` sink on `$!head`, and a scalar attribute is
cell-direct: `self`'s shared attribute cell is the source of truth, while each
frame also keeps an env/local copy. The suspicion is that the compare and the
store do not both go through the cell atomically — the read (`my $orig = $!head`)
and the CAS see different snapshots, so two threads can observe the same `$orig`
and both "succeed". Confirm by instrumenting the `cas` rw-arg writeback path
against `write_self_attr_cell`.

`cas-int.t` fails in the same shape on the `int`-typed variant, so the fix
probably lives in the shared CAS plumbing rather than in the boxed-object path.

Release passing is a narrower window, not a correct implementation — do not close
this on the strength of a green `make roast`. A fix should come with a repro that
also fails in release (raise the thread count or the per-thread node count).

## How it was found

A local `make roast` while landing the Template::Mustache fixes, which at the time
was running against a debug binary.
