# The caller-var writeback drain searches frame-locals outwards

Profiling the HTTP/2 header path of [#7667](https://github.com/tokuhirom/mutsu/issues/7667)
turned up an interpreter-wide cost that has nothing to do with HTTP/2: the drain
of the pending caller-var writeback set was quadratic in a set that only ever
grows.

## The mechanism

`pending_caller_var_writeback` is a work queue of "refresh some frame's local
slot from env". Entries arrive from `$CALLER::x = v` style writers
(`record_caller_var_writeback`), from the shared-variable sync, and — far more
often — from the retain-on-miss branch of `apply_pending_rw_writeback_slow`: a
source that is not a local of the frame currently draining is kept rather than
dropped, because the frame that owns the slot may live further up the stack (the
sibling `submethod BUILD` case pinned by `t/build-sibling-writeback-coherence.t`).

Retain-on-miss has no exit. Nothing removes a name that no frame will *ever* own,
and `merge_method_env` supplies those generously: its `changed_caller_locals`
collects every caller-visible env key a method changed, and loading a module
changes hundreds — enum values, constants, exported symbols, none of which is any
frame's compiled local. Running `Cro::HTTP2::RequestParser`'s header path left
**313 permanently unclaimable entries** (`PROTOCOL_ERROR`, `State::header-c`,
`CBOR::Simple::CBORMajorType::CBOR_Array`,
`&EXPORT::decode-percents::decode-percents`, …).

The drain then looked each of them up with `find_local_slot`, a linear
`code.locals` scan, on **every** call return: about 6000 string comparisons per
return, some 3000 times over four HEADERS frames. In a local `callgrind` run of
that bench at 60 frames — instruction counts, which are deterministic and
load-independent — `apply_pending_caller_var_writeback_slow` was **8.0% of all
instructions executed**, second only to `malloc`, with
`apply_pending_rw_writeback_slow` a further 2.9%.

## The fix

Search frame-locals outwards instead of pending-sources inwards: walk this
frame's `code.locals` once and ask the pending set about each name, rather than
asking `find_local_slot` about each pending source. The set answers in O(1), so
the drain costs O(locals) hash lookups instead of O(pending × locals) string
comparisons — and no longer scales with the accumulated set at all.

The answer is the same. A source names one variable, so no two of them can want
the same slot and the drain order cannot matter; taking slots in index order
still resolves a duplicated name to its first slot, exactly as `find_local_slot`
did; and `HashSet::remove` doubles as the membership test and the
"matched → do not retain" step, so a source is still applied at most once.
`pending_caller_var_writeback` becomes an `FxHashSet<String>` accordingly, which
also makes the dedup-on-insert every producer performs O(1) instead of linear in
the accumulated size.

## What was tried and rejected

Dropping an unclaimed source when the drain reaches the outermost frame — on the
reasoning that no ancestor is left to own the slot — looks right and is not. A
proxy-bound writeback is recorded while a `code` that does not carry the slot is
current, and is claimed by a *later* drain in the very same frame:

```raku
my $str = "gorch ding";
my $r := substr-rw($str, 0, 5);
$r = "gloop";
is $str, "gloop ding";     # t/substr-rw-lvalue-writeback-coherence.t
```

That test caught it. The names therefore still accumulate; what stops costing is
looking at them.

## What it buys, and what it does not

`t/caller-writeback-drain-coherence.t` pins that every writeback a real frame can
claim still arrives: across an intervening deeper call, two frames up with a miss
in between, sibling `BUILD` submethods, captured-outer mutation from a method
(with and without a nested call inside it), `is rw` parameters, a repeatedly
called closure, and after an `enum` registration.

Measured the same deterministic way, an A/B of two release builds over the same
bench at 12 frames:

| | before | after |
| --- | --- | --- |
| `apply_pending_caller_var_writeback_slow` | 112.5M Ir (1.27%) | 0.67M Ir (0.01%) |
| `apply_pending_rw_writeback_slow` | 75.0M Ir (0.84%) | 9.3M Ir (0.11%) |
| whole program | 8.888G Ir | 8.666G Ir (-2.5%) |

The second row falls out of the same change: that drain's retain-on-miss arm
tested set membership before pushing, which was the same linear scan.

(The share the two functions take grows with the number of frames — 8.0%/2.9% at
60 frames against 1.27%/0.84% at 12, where process start-up is a larger slice —
so the whole-program figure above is the conservative end.)

It does **not** move #7667's headline number. A HEADERS frame still costs ~25 ms,
and the per-stage breakdown recorded on that issue shows why: the cost is spread
across HPACK decoding, `Cro::HTTP::Request` construction, the per-stream
`whenever` registration and the `supply` plumbing, with no single dominant term.
That issue stays open with the measurements attached.
