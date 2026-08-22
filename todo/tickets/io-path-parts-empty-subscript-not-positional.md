# `$parts[]` (empty postcircumfix index) on `IO::Path::Parts` doesn't iterate its positional elements

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/IO/Path/Parts.rakudoc:71`).

## Repro

```raku
my $parts = IO::Path::Parts.new('C:', '/some/dir', 'foo.txt');
.say for $parts[];
```

- `raku`:
  ```
  volume => C:
  dirname => /some/dir
  basename => foo.txt
  ```
- `mutsu` (`target/debug/mutsu`): a single line — the whole object's default gist:
  ```
  IO::Path::Parts.new("C:","/some/dir","foo.txt")
  ```

Verified directly. Also verified this is `IO::Path::Parts`-specific, not a general
"empty subscript" gap — a plain `Array` already works correctly:

```
$ target/debug/mutsu -e 'my @a = 1,2,3; .say for @a[];'
1
2
3
$ target/debug/mutsu -e 'my $parts = IO::Path::Parts.new("C:", "/some/dir", "foo.txt"); .say for $parts[];'
IO::Path::Parts.new("C:","/some/dir","foo.txt")
```

Earlier lines in the same doc example (`$parts<volume>`, `$parts[0]`,
`$parts[0].^name`) all already match raku — only the bare `$parts[]` ("all positional
elements") form fails.

## Root cause hypothesis

`IO::Path::Parts` is documented (`raku-doc/doc/Type/IO/Path/Parts.rakudoc`) as both
`Associative` (keyed access via `<volume>` etc.) and `Positional` (indexed access via
`[0]`, and here, "all elements" via `[]`) — it presumably wraps 3 key/value pairs. The
single-index form `$parts[0]` works, so positional dispatch exists at some level, but
the bare/empty-index form (which should mean "give me all positional elements as a
list", the same as it does for `Array`) isn't recognized for this type and falls back
to treating `$parts[]` as if it were just `$parts` (hence the whole-object gist).

## Affected files (starting point)

- Wherever `IO::Path::Parts` is implemented (grep for `"IO::Path::Parts"` in
  `src/runtime/`, `src/builtins/`) — check its positional/subscript dispatch,
  specifically the no-index-given case, versus how plain `Array`'s `@a[]` is handled.
