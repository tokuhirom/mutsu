# A custom `.gist` method on a role-mixed (`does`/`but`) native value is skipped when the value is gisted inside an array/list

Discovered via the doc-diff harness on `raku-doc/doc/Language/typesystem.rakudoc` (around line
657, the SI-unit `Unitish` example: `say [75kg, N(75kg)]` prints raw numbers instead of
`75kg`/`735.49875kN`).

## Repro

```
role Tag { method gist { "TAGGED:" ~ self } }
my $x = (5) does Tag;
say $x;
say [$x];
```

- raku: `TAGGED:5` then `[TAGGED:5]`
- mutsu: `TAGGED:5` then `[5]` — the custom `.gist` from the mixed-in role is honored when the
  value is gisted directly, but is bypassed when the value is an element being gisted as part of
  an array/list.

## Root cause guess

Array/List gisting likely has its own per-element stringification path that dispatches based on
the element's *base* type (numeric formatting fast path) rather than going through the normal
method-dispatch `.gist` lookup that would find the role-mixed override.

**Possibly the same underlying root cause as**
[list-but-role-loses-positional-binding.md](list-but-role-loses-positional-binding.md) and
[hash-default-role-mixin-dropped.md](hash-default-role-mixin-dropped.md) — see that ticket's
note on the shared hypothesis ("a `but`/`does`-mixed value's role metadata doesn't survive a
generic storage/dispatch path"). Filed separately because each has a distinct minimal repro;
investigate together and merge into one PR if a single fix site is found.

## Affected files (starting point)

- `src/runtime/` — array/list `.gist` implementation (look for a fast-path numeric formatter
  used when gisting collection elements)

## Suggested next step

Find where `[$x].gist`/`say [$x]` iterates elements and calls each one's stringification —
check whether it calls the generic method-dispatch `.gist` (which would find the role mixin) or
a native fast-path formatter that only looks at the element's underlying primitive type.
