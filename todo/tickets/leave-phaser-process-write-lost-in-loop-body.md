# A `LEAVE`-driven `PROCESS::` write is lost specifically inside `for`/`while` loop bodies

Split off from `news/2026-08/leave-phaser-if-given-not-firing.md` (2026-08-18),
which fixed the more severe sibling bug: a `LEAVE` phaser directly inside an
`if`/`given` block never fired *at all*. This ticket is the narrower residual
left after that fix — `for`/`while` already correctly RUN their `LEAVE`
phaser (confirmed with a plain side-effect counter), but a `LEAVE`-driven
write to a `PROCESS::` (dynamic pseudo-package) variable specifically does
not propagate to code that runs after the loop exits.

## Repro

```raku
my $ran = 0;
for 1 {
    LEAVE $ran++;
}
say $ran;                       # raku: 1   mutsu: 1  (LEAVE genuinely fires — not this bug)

PROCESS::<$X> = 42;
for 1 {
    LEAVE PROCESS::<$X> = Nil;
    say "inside: ", PROCESS::<$X>.WHAT;   # both: (Any) -- correct while inside
}
say "after: ", PROCESS::<$X>.WHAT;        # raku: (Any)   mutsu: (Int) -- write lost
```

`while` shows the identical pattern (verified with `while $done < 1 { LEAVE
PROCESS::<$X> = Nil; ...; $done++ }`).

## Where this differs from the `if`/`given` fix

`if`/`given` bodies with phasers now route through the SAME `OpCode::BlockScope`
mechanism sub bodies and bare blocks already use correctly (`compile_phaser_block_scope`,
gated by `has_block_enter_leave_phasers`). Loop bodies (`for`/`while`) use a
DIFFERENT, older mechanism — `expand_loop_phasers` (`src/compiler/helpers_phasers.rs`),
which splits `ENTER`/`LEAVE`/`KEEP`/`UNDO`/`FIRST`/`NEXT`/`LAST` into separate
statement lists and splices them around the loop body directly (not through
`OpCode::BlockScope`). Since the LEAVE phaser genuinely runs (confirmed via a
plain counter side effect), this is not a "phaser never fires" bug like the
`if`/`given` one — it's specifically that a write to a `PROCESS::`/dynamic
pseudo-package variable made from inside that spliced-in LEAVE body doesn't
reach whatever the REST of the program later reads through the normal
`PROCESS::` lookup chain. Likely candidates (not yet investigated):

- The spliced LEAVE body might execute in a different env/scope-overlay
  frame than the loop's main body, and `PROCESS::` writes might be routed
  through a frame-relative mechanism (`dynamic_pseudo_stash_entries`, per
  the ORIGINAL Log::Timeline investigation this whole family of tickets
  descends from) that doesn't correctly identify the right target frame for
  a write made from the spliced-out LEAVE list specifically.
- The loop's own per-iteration scope-cloning (needed for `state` semantics
  inside a loop) might snapshot/restore the dynamic-var stash in a way that
  discards the LEAVE's write once the loop's iteration scope is torn down.

## Repro file

```sh
cargo build
timeout 15 target/debug/mutsu <(cat <<'EOF'
PROCESS::<$X> = 42;
for 1 {
    LEAVE PROCESS::<$X> = Nil;
}
say PROCESS::<$X>.WHAT;
EOF
)
# raku: (Any)   mutsu: (Int)
```

## Severity

Low-to-moderate: narrow (only a `PROCESS::`/dynamic-var write made specifically
from a loop-body `LEAVE`, not general `LEAVE` side effects, which already
work), but a real, general interpreter gap independent of `Log::Timeline` —
the original module this whole investigation descends from does not
exercise loop-body LEAVE writes at all (`t/logging.rakutest`'s `.task`/`.start`/
`.end` blocks use `given`, not `for`/`while`), so this ticket's own repro is
purely synthetic; not yet confirmed to block anything concrete.

Affected: `src/compiler/helpers_phasers.rs` (`expand_loop_phasers`), and
whatever env/dynamic-scope mechanism `PROCESS::` writes route through at
runtime (`dynamic_pseudo_stash_entries` per the earlier investigation, not
yet located precisely for this specific loop-body case).
