# Method-local reads leak a same-named closure-captured mainline lexical (90_csv 495)

Found 2026-08-12 while closing out the 90_csv.t frontier; this replaces the
earlier diagnosis "gather takes are lost after a Callable-in/Array-out csv
call" — a full instrumentation pass proved the gather machinery INNOCENT (the
collector stack balances perfectly and every take lands and pops with the
right counts). The bug is a **name-resolution scope leak**, and the earlier
"poison call" theory was wrong too: the leak fires on every call; the prior
csv call merely moved a file handle to EOF, which is what made it visible.

## Symptom chain (all verified by probes)

t/90_csv.t test 495 ("data from CODE/AR" got `[]`) and the end-of-file abort:

1. In Text::CSV's `method CSV`, the Callable-in arm's
   `@in = gather while $in() -> $r { take $r }` works: post-assign probe shows
   `@in.elems == 3`.
2. `@in` is then CLOBBERED by the next paragraph:
   `if ($io-in ~~ IO::Handle and $io-in.defined) { @in = ... getline_all ($io-in ...) }`.
   The method declares `my IO::Handle $io-in;` and, for a Callable `in`,
   never assigns it — yet the probe shows `defined=True what=(Handle)`:
   **`$io-in` reads as the TEST SCRIPT's own `my $io-in = open $fni, :r;`**.
   That handle sits at EOF (an earlier csv call read it — same leak — and
   nothing seek'd it back), so `getline_all` returns `[]`, the out-file is
   written empty, and the read-back comparison fails.

## Minimal trigger (13 lines, reduced from the test with a strict oracle)

    use Slang::Tuxic;
    use Text::CSV;
    my $fni    = "_90in.csv";
    my $io-in  = open $fni, :r;
    sub sleep-time {
        $io-in.seek (0, SeekFromBeginning);
        }
    my int $idx2 = 0;
    my $rows2 = [[1],[2]];
    sub getrow2 { return $rows2[$idx2++]; }
    my $r = csv (in => &getrow2);   # inside CSV, $io-in reads as the open handle

The load-bearing ingredient is `sub sleep-time` — a mainline sub (NEVER
CALLED) that closes over `$io-in`. Without it, the mainline `my $io-in` stays
slot-only and the method's reads are clean (`defined=False`). With it, the
mainline assignment goes through `set_env_plain_lexical` →
`set_shared_var_sym` (the closure capture surfaces the lexical), and the
method's reads of its own `$io-in` start seeing the mainline handle.

A small hand-built imitation (module with a class whose method declares
`my IO::Handle $io-in;`, mainline capture, wrapper sub) does NOT reproduce —
some additional property of the Text::CSV compile (method size, slang, or
which opcode the reads compile to) is required; the reduced script above is
the reliable repro.

## Interpreter-side forensics (instrumented run, all sites env-gated)

- `Env::insert`/`insert_sym`: the ONLY IO::Handle insert for key `io-in` in
  the whole run is the mainline assignment (via `set_shared_var_sym`). No
  Handle is ever inserted into the method frame env.
- `Env::remove_sym`: never called for `io-in`.
- `Env::get_sym` at the failing check returns `Any` (via the method frame
  OVERLAY — the declaration's entry is intact and shadowing correctly).
- `GetLocal` of the method's `io-in` slot (slot 29) returns `Nil`/`Any` —
  never a Handle.
- `GetUpvalue` (both the array hit and the by-name fallback): never fires
  for `io-in`.

Yet the module-level reads (`$io-in.defined` in the `if` condition and in a
`note` interpolation) see the Handle. So the leaking read path is NONE of
env-get / GetLocal / GetUpvalue — it resolves through the
**shared-var / main-alias layer** (`get_shared_var` /
`get_env_with_main_alias`'s `unit_lexicals` handling — the "module sub's free
read of its own compunit's file-scope `my`" mechanism, see the NB comment in
`exec_get_upvalue_op`, src/vm/vm_var_assign_local_get.rs). The exact opcode
whose evaluation consults that store for this read was not yet pinned —
identifying it is the first step of the fix (candidates: string-interpolation
free-var reads, smartmatch RHS regex-interpolation sync
`sync_regex_interpolation_env_from_locals`, GetGlobal's fallback chain).

## Why this is deep

The shared-var/main-alias store is keyed by bare name with no lexical-scope
qualifier: once a file-scope `my $x` is closure-captured (promoted into the
store), ANY later by-name-fallback read of `$x` — including one inside a
module method that declared its own `my $x` — can resolve to the mainline
value. That is a general cross-compunit scope leak, not a CSV bug. The fix
needs either scope-qualified keys in the store or a guarantee that a frame
with its own declaration never reaches the by-name fallback (slot/env must
answer first) — ADR-0018 campaign territory (see memory
`env-writeback-campaign-state-sync-bug` for the family).

## Repro assets

- `tmp/text-csv` (github.com/Tux/CSV clone) + the 13-line script above
  (was `tmp/ioin-leak-min.t`; recreate from this file if tmp/ is gone).
  Oracle: probe `$io-in.defined` inside method CSV's io-in check — or just
  observe `csv (in => &getrow2)` return `[]`/wrong rows when the handle is
  at EOF.
- Full suite: `MUTSU_FUDGE` not needed (not roast);
  `prove -e "mutsu -I lib" t/90_csv.t` fails 495 + end abort.
- 90_csv.t is otherwise 494/496 (test 159 fails under rakudo too —
  raku-parity, not a mutsu bug).

## Impact

Blocks 90_csv.t (the last real mutsu failure in it). The underlying leak can
corrupt any program where a module method's local shares a name with a
closure-captured script lexical — worth fixing on general grounds.
