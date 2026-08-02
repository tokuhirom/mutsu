# HTTP::Server::Tiny — the async-serving remainder

Extracted from PLAN.md §1 B4 (2026-08-02). Basic serving works end to end; these three do not fire in
the default configuration, which is why they are still open.

1. **Keep-alive consecutive requests** on one connection.
2. **Chunked request bodies**.
3. **`done` / `last` control signals inside `whenever $conn.Supply(...)`** — the tap callback runs on
   a worker thread, disconnected from the `react` control-flow frame, so a control signal raised
   there does not reach the block that owns the `react`.

(3) is the general one: it is the same worker-thread-vs-control-frame disconnect as the other
`whenever` tickets ([whenever-owned-lexical-outlives-the-react-block.md](whenever-owned-lexical-outlives-the-react-block.md),
[schedule-on-whenever-env-loss.md](schedule-on-whenever-env-loss.md)), and it is what
[ADR-0008](../../docs/adr/0008-push-based-supply-event-delivery.md) set the delivery model for.

## Related, separate

**Full Humming-Bird serving** (LOAD + LISTEN + accept + decode already work, #3549) has two blockers
of its own:

- **B1** — leakage of `var_type_constraint` from typed parameters to same-named *caller* lexicals.
  The proper fix scopes the global name-keyed HashMap at call boundaries; making env authoritative is
  not possible because it breaks subset-6e.
- **B2** — a detached `start { react { whenever $chan { } } }` is not driven unless awaited: the
  concurrency-scheduling campaign, i.e. [shared-worker-pool-adr.md](../deep/shared-worker-pool-adr.md).

Humming-Bird is **not** the web-framework target any more (Cro is — see
[docs/batteries/web-framework.md](../../docs/batteries/web-framework.md)), so B1/B2 matter as
general interpreter bugs, not as a battery gate. B1 in particular is a plain correctness bug worth
fixing on its own.
