# Creating a closure or a `gather` no longer costs one copy of every type and constant in scope

A closure capture keeps the closure's free variables and every visible system
name: types, constants, dynamics, uppercase lexicals and `__mutsu_*`
metadata (ADR-0094). The free variables were already probed by name (#9246).
The system names were still copied one by one into a fresh map on every
`-> { }`, every block literal and every `gather`, so a program with a
thousand constants paid for all of them on each closure it created.

They are now shared. Each env tier memoizes the system names a capture keeps
from it, as an immutable tier of its own (`Tier::capture_sys`). A write to a
plain lexical, the topic, `$/` or `$!` leaves the memo alone, so a loop that
writes its own variables keeps the same memo. A capture is a small tier of its
own (the free variables, the volatile names and the creating frame's narrow
overlay) over a stack of those shared layers (`CaptureView`,
`Env::layered_capture`).

The call frame installs that stack as its capture fallback (ADR-0092). The
consumers that iterate a captured env see the whole capture, folded on first
ask. This means substitution replacement blocks, END phasers, the `xx` thunk
and where-constraint merges behave as before. The decision and its precedence
rules are recorded in ADR-9170.

The one-entry capture memo (`vm_capture_cache`) is gone. It could only hit
while no tier of the creating chain was written by name, and the layered
capture no longer needs it. A `gather` force also no longer folds its captured
env. Its write-back merge walks only the env's own overlay, where every write
of its body lands, instead of string-comparing every captured name on each
force.

20000 creations, with N declarations in scope (release, `MUTSU_JIT=off`), time
at N = 1000 → N = 2000:

| in scope | `-> { $q }` before | after | `gather { take 1 }` before | after |
| --- | --- | --- | --- | --- |
| `my class` | 0.40 → 0.62 s | 0.042 → 0.039 s | 0.39 → 0.70 s | 0.070 → 0.071 s |
| `constant` | 0.62 → 1.37 s | 0.053 → 0.058 s | 0.70 → 1.35 s | 0.085 → 0.104 s |
| `my $*d` | 1.07 → 1.91 s | 0.040 → 0.040 s | 0.95 → 2.14 s | 0.090 → 0.071 s |
| `my $Upper` | 0.33 → 0.63 s | 0.040 → 0.038 s | 0.39 → 0.75 s | 0.077 → 0.100 s |

While measuring this, a `for` loop turned out to pay O(state locals of the
whole chunk) per iteration in `sync_state_locals_in_range`. That is filed as
#11347.
