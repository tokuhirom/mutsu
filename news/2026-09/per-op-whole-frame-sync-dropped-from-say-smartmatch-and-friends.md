# `say`, `~~`, `$outer = ...`, `but`/`does`, `andthen`, user `sink` and `eager` stop paying for every local in the frame

A cluster of opcodes bracketed their own work with a full synchronisation of
the running frame's locals against `env`: a whole-frame broadcast before the
op, a whole-frame snapshot and diff around a re-entrant call, or a scan of
every env key. Each execution therefore cost O(L) (L = the code unit's local
slots) or O(v) (v = env entries) on top of the op's own work, whatever its
operands were. In a long mainline script every `say` and every `~~` paid for
every other variable ([#9169](https://github.com/tokuhirom/mutsu/issues/9169)).

What each op does now:

- **`say`/`put`/`print`/`note`** dropped their pre-sync entirely. It existed so
  a `$*OUT` override or a user `.gist` would see fresh values of outer
  lexicals, but those are method bodies, and a method body reads its free
  variables through the per-store mirror (or a shared cell), exactly as it
  does when called any other way.
- **`~~`** publishes only what the regex engine can read by a runtime-parsed
  name. A new module (`src/vm/vm_smartmatch_sync.rs`) scans the RHS's regex
  literals, and after the RHS has run, a regex *value* it computed
  (`$x ~~ $re`), for the variables their patterns can name. It publishes just
  those slots. A type, constant, range, callable or instance RHS publishes
  nothing. A regex that embeds code (`{ }`, `<?{ }>`, `:my`), a destructive
  `s///`/`tr///`, and a junction or collection RHS keep the whole-frame
  publish. The `$/`-is-a-local test and the match-variable writeback now go
  through the chunk's slot index instead of scanning `code.locals`.
- **`SetGlobal`**'s reverse alias propagation (`my $c := $_; $_ = 5` must
  update `$c`) used to scan every env overlay key for a
  `__mutsu_sigilless_alias::` entry naming the target. A process-global
  reverse index (`src/sigilless_alias_index.rs`) now answers that question.
  It is fed at the env tier's single insert funnel, so no alias-creating
  site can slip past it. It is an append-only superset, so each candidate is
  re-checked against the live env entry. The attribute-twigil slot scan on
  the same path became a slot-index probe.
- **`but`, `does`, a user `.defined` behind `andthen`/`orelse`, and a user
  `sink`** replaced their whole-frame sync + snapshot + diff with the drain an
  ordinary method call uses (`apply_pending_caller_var_writeback`, O(1) when
  nothing was recorded). A captured-and-mutated lexical is a shared cell the
  caller's slot already observes.
- **`eager`** over a `gather` reconciles only the slots a `gather` body of
  the chunk can name (`env_consumer_slots.gather_list`, computed at compile
  time), not every local.

Measured with `scripts/vm-complexity-check.sh 'frame locals' SetGlobal`
(release build; fixed body, frame size N -> 2N; this session's local runs,
before -> after, first column in seconds at N):

| case | before t(N) | ratio | after t(N) | ratio |
|---|---:|---:|---:|---:|
| `say 1` x2000, N = 500 locals | 0.102 | 1.92 | 0.0018 | 1.00 |
| `$x ~~ Int` x20000, N = 250 | 0.636 | 1.95 | 0.036 | 1.15 |
| `$s += (my $z = $_)` x20000, N = 2000 | 0.441 | 1.67 | 0.017 | 1.10 |
| `my $y = 5 but True` x20000, N = 500 | 0.341 | 1.85 | 0.082 | 1.08 |
| `$y does R` x5000, N = 500 | 0.392 | 1.96 | 0.027 | 0.77 |
| `$o andthen 1` (user `.defined`) x20000, N = 500 | 0.649 | 1.80 | 0.130 | 1.11 |
| `eager gather { take 1 }` x5000, N = 2000 | 1.189 | 2.14 | 0.010 | 1.11 |
| `$k.self` sunk (user `sink`) x20000, N = 1000 | 1.166 | 1.85 | 0.146 | 0.99 |

The script gained cases for `eager`, `but`, `does`, `andthen` and a user
`sink` (the latter measured as `$k.self` in sink context, because `K.new`
itself still grows with the frame, which is a constructor cost outside this
issue). `t/vm/writeback/per-op-sync-free-coherence.t` pins every by-name reader and
writer the removed syncs used to cover.

Still open under #9169: `&f` (`GetCodeVar`) hands back a `Sub` that shares
the current env tier, so the frame's next env write copies the whole tier.
Fixing that means changing what a named routine's code object captures. A
`~~` whose RHS is a code-bearing regex or a junction/collection still pays
the whole-frame publish.
