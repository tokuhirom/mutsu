# A named `sub` called from inside a `for` loop body reads the loop's parameter dynamically instead of resolving its own lexical closure

## TL;DR

A `sub` declared **before** (and lexically outside) a `for LIST -> $x { ... }`
loop, when called from inside that loop's body, incorrectly reads the loop's
*current per-iteration* value of `$x` if the sub's own free variable happens
to share that bare name — instead of resolving to whatever `$x` meant in the
sub's own enclosing (lexical) scope.

## Repro

```raku
my $client = "outer";

sub helper($unrelated) {
    start { $client };
}

my @promises;
for 1, 2 -> $client {
    @promises.push(helper($client));
}
say (await @promises).join(',');
```

- `raku`: `outer,outer` — `helper`'s `$client` is lexically the top-level `my
  $client = "outer"`; the loop's `-> $client` parameter is a completely
  separate binding in a scope `helper` cannot see (`helper` was compiled/
  declared before the loop even exists).
- `mutsu` (main, `65844e560`): `1,2` — `helper`'s body reads whatever value
  the *calling* loop iteration currently has bound to the bare name `client`
  in the dynamic env, not its own lexical closure.

## Discovery context

Found while writing a verification probe for
`docs/adr/0023-binding-provenance-spawn-capture.md`'s Step 3 ("confirm a
callee's own free variable that merely shares an outer loop's parameter name
is not mistaken for that loop's own per-iteration binding"). Confirmed
independent of ADR-0023's change: reproduces identically on `main` before
that ADR's patch (`git stash` verified), so it is a pre-existing,
unrelated bug in mutsu's name-keyed env / dual-store lexical resolution, not
something ADR-0023 introduced or is responsible for fixing.

## Likely root cause (not yet investigated in depth)

mutsu's env is a name-keyed dynamic structure (PLAN.md §6 / the dual-store
work); a `sub`'s free-variable reads apparently fall back to a same-named env
lookup that finds whatever is currently bound under that bare name in the
active call chain, rather than resolving through the sub's own captured
lexical scope (the way a block/closure's `free_var_syms` /
`free_var_parent_slots` machinery does — see `runtime/runtime_thread.rs`'s
`block_captured_scalars` for the equivalent mechanism on closures). Whether
named subs get the same free-variable-capture treatment as blocks/closures at
all needs investigation — this may be a gap specific to top-level/lexically
hoisted named `sub` declarations vs. anonymous blocks.

## Why this is a ticket, not a quick fix

This touches the same dual-store / lexical-scope machinery flagged elsewhere
as a deep, high-blast-radius area (see PLAN.md §6, MEMORY.md's "Slice F /
dual-store" campaign). It needs investigation into how named `sub`s resolve
free variables today (vs. blocks) before a fix can be scoped.

## Verification (once fixed)

- The repro above should print `outer,outer` under mutsu, matching `raku`.
