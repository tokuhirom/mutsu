# Attribute order in a `.new`/gist under multiple inheritance is wrong (deterministic, not hash-order noise)

Discovered via the doc-diff harness on `raku-doc/doc/Language/objects.rakudoc` (around line
1185).

## Repro

```
class Bull { has Bool $.castrated = False; }
class Automobile { has $.direction; }
class Taurus is Bull is Automobile { }
say Taurus.new;
```

- raku: `Taurus.new(castrated => Bool::False, direction => Any)`
- mutsu: `Taurus.new(direction => Any, castrated => Bool::False)`

Confirmed deterministic across 3 repeated runs — this is not hash-iteration-order noise.
Attribute order in a gist should follow MRO/declaration order (parents in linearization order,
each parent's own attributes in declaration order), and mutsu has the two parents' attribute
groups swapped.

## Root cause guess

Wherever mutsu assembles an instance's attribute list for `.gist`/default stringification under
multiple inheritance (`is Bull is Automobile`), it's likely iterating parents in reverse MRO
order, or building the attribute list by walking the class hierarchy in a different direction
than the C3 linearization used elsewhere for method dispatch.

## Affected files (starting point)

- `src/runtime/class.rs` — attribute-list assembly for multi-inheritance classes, `class_mro`
- Wherever `.gist`/default `Instance` stringification enumerates attributes

## Suggested next step

Check whether `class_mro(Taurus)` itself returns `[Taurus, Bull, Automobile, ...]` in the
correct order, then check if the gist-attribute-collection code walks that MRO list forwards or
backwards (or uses a separate, differently-ordered mechanism entirely).
