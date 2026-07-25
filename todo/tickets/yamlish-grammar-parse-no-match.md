# YAMLish's grammar matches no input under mutsu

Blocker #3 for the `YAMLish` battery candidate (`docs/batteries/yaml.md`),
reached after #1 (`=>` currying, #5432), #1.5 (placeholder-in-WhateverCode) and
#2 (module-local type shadowing) are all applied.

With those three fixes, `use YAMLish` loads and `Grammar.parse($input)`
**dispatches** — but the grammar then matches nothing, so every `load-yaml`
returns a `Failure`:

```raku
use YAMLish;
say load-yaml("42").raku;        # raku: 42            mutsu: Failure "Couldn't parse YAML"
say load-yaml("foo: 1").raku;    # raku: ${:foo(1)}    mutsu: Failure
say load-yaml("- a\n- b").raku;  # raku: $["a", "b"]   mutsu: Failure
```

Even the trivial single-scalar input fails, so this is not input-specific — the
grammar's `TOP` fundamentally does not match under mutsu.

## Where to look

The YAML grammar is `grammar Grammar` at lib/YAMLish.rakumod:150–783 — large and
action-heavy, with a four-grammar schema hierarchy used for scalar resolution
(`grammar Schema::JSON`, `grammar Schema::Core is Schema::JSON`,
`grammar Schema::Extra is Schema::Core`). `TOP` (line 163) uses:

- named captures with an explicit alias and a subrule
  (`<document=directive-document>`, `<document=simple-document>`);
- non-capturing subrule calls (`<.document-prefix>`, `<.document-suffix>`);
- quantified alternations across document forms.

`load-yaml` also does scalar resolution via `$schema.new.parse($!value)`
(line 80) against the `Schema::*` grammars, and `Grammar.parse($input)` for the
document (line 944).

Concrete candidate features to check first (the `Schema::*` grammars lean on all
of these, and the document `Grammar` on the capture/boundary ones):

- **`proto token element { * }` + `token element:<null>` / `:<int>` / …** — proto
  tokens with `:<sym>`-based multi dispatch, **extended across grammar
  inheritance** (`Schema::Core is Schema::JSON` adds more `element:<sym>`
  candidates to the inherited proto). This is the highest-risk feature.
- **`<|w>`** word-boundary assertion.
- **`$<sign=[+-]>?` / `$<value>=[ … ]`** — named captures bound to a character
  class / bracketed pattern.
- **`<name=subrule>`** aliased subrule captures and non-capturing `<.subrule>`.
- **`:16(~$<value>)`** radix conversion in an action (only reached once parsing
  works).

Since even `load-yaml("42")` fails, start with the **document `Grammar`**
(line 150) `simple-document`/scalar path, which is what `Grammar.parse("42")`
exercises — before chasing the `Schema::*` proto-token machinery.

## Next step (needs the #1/#1.5/#2 stack applied)

Reduce which construct fails: build a minimal grammar exercising
`<name=subrule>` aliased captures, non-capturing `<.subrule>`, and a
`Schema::Core is Schema::JSON` inheritance chain with inline `{ make … }`
actions, and bisect toward the smallest `TOP` that matches under raku but not
mutsu. It may be one grammar-feature gap or several; the module now loads, so the
grammar can be exercised directly:

```sh
mutsu -I <yamlish-lib> -I modules/MIME-Base64/lib \
  -e 'use YAMLish; say load-yaml("42").raku'
```

(Requires a build carrying #5432 + the placeholder-in-WhateverCode fix + the
module-local-shadow fix; on a plain checkout the module does not yet load.)
