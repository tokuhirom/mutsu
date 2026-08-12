# Method wraps cannot be removed: handle.restore silently no-ops, .^lookup(...).unwrap throws

Found by the ADR-0019 E9-pre raku verification campaign (2026-08-12, Rakudo v2026.06).
Sub-side wrap/unwrap round-trips work (`t/wrap.t`, `t/routine-unwrap-error.t`); the METHOD side
is broken in both removal forms.

## Divergence

```raku
class C { method m() { "orig" } }
my $h = C.^lookup('m').wrap(-> |c { "w-" ~ callsame });
say C.new.m;    # both: w-orig
$h.restore;
say C.new.m;    # raku: orig    mutsu: w-orig (restore silently did nothing)
```

```raku
my $h2 = C.^lookup('m').unwrap($h2-from-wrap);
# raku:  unwraps, later calls run the original
# mutsu: dies "Cannot unwrap a sub that has not been wrapped"
```

## Root cause (already surveyed by ADR-0019 E10's design)

`todo/deep/adr0019-e8-e11-candidate-sequence-semantics.md` (E10 facts + design decision 4)
records this exact gap: `.unwrap`/`restore` never remove `method_wrap_chains` entries — only
class redeclaration purges them (`registration_class_validate.rs:377`) — and the two
method-wrap mutation paths invalidate nothing. The unwrap error comes from the sub-side unwrap
looking for the handle in the sub-keyed `wrap_chains`, where a method wrap (keyed by
`(class, method, candidate_idx)` in `method_wrap_chains`) never lives.

## Fix route

This is scoped to be fixed BY ADR-0019 E10a ("registry-owned method wraps + generation bumps on
all wrap mutations (+ the unwrap leak fix)"). If E10a is far off, an interim fix teaching
`.unwrap`/`.restore` to also search-and-remove `method_wrap_chains` entries is self-contained.
raku-verify `.unwrap` edge semantics (out-of-order removal on methods, double restore) first —
the sub-side tests cover those shapes for subs only.

The E9-pre pin for this lands with the fix.
