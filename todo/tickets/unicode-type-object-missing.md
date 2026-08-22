# The `Unicode` type object doesn't exist — bareword `Unicode` resolves to a plain `Str`

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Unicode.rakudoc:27,37`).

## Root cause

Raku has a built-in `Unicode` class (a singleton-style type used to query the Unicode database
version and normalization-form settings the running VM uses, via class methods like `.version`,
`.NFG`, `.NFC`, `.NFD`, `.NFKC`, `.NFKD`). mutsu does not implement this type at all: the bareword
`Unicode` falls back to being interpreted as a plain string.

```raku
say Unicode.^name;
```
- `raku`: `Unicode`
- `mutsu`: `Str`

Calling any class method on it then fails, since mutsu is really calling a `Str` method:
```raku
say Unicode.version; # raku: v17.0 (doc says v15.0, drifted since); mutsu: No such method 'version' for invocant of type 'Str'
say Unicode.NFG;      # raku: True; mutsu: No such method 'NFG' for invocant of type 'Str', "Did you mean 'NFC'?"
```

(Note: `Unicode.version` itself is `raku-drift` relative to the doc's stated `v15.0` — current
`raku` reports `v17.0` — but the underlying bug, that mutsu has no `Unicode` type at all, is real
and independent of which Unicode DB version is the "right" answer.)

## Minimal repro

```raku
say Unicode.^name;
```
- `raku`: `Unicode`
- `mutsu`: `Str`

## Affected files (starting point)

- Wherever mutsu's other built-in singleton-style types are registered (e.g. how `NFC`/`NFD`
  normalization-form values or similar core type objects are set up) — search for existing
  `Unicode`-related builtins (unicode.rs / `builtins/unicode.rs`) to see whether the underlying
  version/NFG-query data already exists and just needs a `Unicode` type object + class methods
  wired to it, versus needing new plumbing entirely.
