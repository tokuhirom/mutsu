# Array/Hash elements lose itemization when bound to a `$` scalar (loop params, element reads)

## Symptom

`CSV::Table`'s `t/5-save.t` dies in `save`:

```
Your printf-style directives specify 3 arguments, but 4 arguments were
supplied to format '%-*.*s'.
```

The module does `for @!cell.kv -> $i, $v { sprintf "%-*.*s", $w, $w, $v }`
where each `$v` is a row (an Array). In raku, `$v` is an *item* (an element
container bound to a `$` parameter), so sprintf receives it as ONE argument
and stringifies it. In mutsu the Array arrives bare and the sprintf slurpy
flatten (`flatten_into_slurpy`, which correctly respects itemization when it
is present) explodes it into its elements — wrong arg count.

## Minimal repro

```
$ target/debug/mutsu -e 'my @c = [<a b>], [<c d>]; for @c.kv -> $i, $v { say sprintf "%-*.*s", 5, 5, $v }'
Your printf-style directives specify 3 arguments, but 4 arguments were supplied ...
$ raku -e 'my @c = [<a b>], [<c d>]; for @c.kv -> $i, $v { say sprintf "%-*.*s", 5, 5, $v }'
a b
c d
```

The underlying divergence is visible without sprintf:

```
$ target/debug/mutsu -e 'my @c = [<a b>],[<c d>]; for @c -> $v { say $v.raku }; my @d = @c; say @d[0].raku'
["a", "b"] / ["c", "d"] / ["a", "b"]
$ raku -e '...'
$["a", "b"] / $["c", "d"] / $["a", "b"]
```

i.e. mutsu's element values are not itemized anywhere: not in the `for`/`.kv`
`$`-param binding, and not on `@d[0]` reads. `my $v = [1,2]` DOES itemize
(scalar assignment goes through `itemize_scalar_store`), so the gap is
specifically *element* reads / `$`-param *binding*, not scalar assignment.

## Why this is deep, not a ticket

Raku's model is that array/hash **elements are Scalar containers**; anything
read out of one and bound to a `$` name is an item. mutsu stores elements as
bare values, so itemization would have to be (re)applied at every element
read / param-bind boundary — or elements become real containers, which is
exactly ADR-0001's Track B ("element `ContainerRef` cells", §2.1), explicitly
fused with the GC campaign and NOT to be started standalone. A shallow
alternative — itemizing at `$`-sigil param binding (`for ... -> $v`, `.kv`,
`.map`) and at `Index` reads feeding scalar contexts — would fix `.raku`
output and the sprintf flatten, but it touches every loop/param path and
needs a survey of tests that (incorrectly or not) rely on the current bare
values flattening; do it as its own measured campaign, not as a drive-by.

## Affected

- `CSV::Table` `t/5-save.t` (last remaining failure in that suite; 9/10 files
  pass as of the `WrapVarRef` shadow-slot fix, see
  `news/2026-08/csv-table-comment-strip-loop-var-state-sync.md`).
- Every `.raku`/`.gist` of arrays-of-arrays read back element-wise
  (`$[...]` vs `[...]` — the `.raku` residues family, PLAN §8 QA).

## Verification once fixed

`cd ~/.zef/store/CSV-Table-0.0.2/*/ && prove -e '<mutsu> -I lib -I <Font-AFM>/lib -I <Text-Utils>/lib -I <AlgorithmsIT>/lib' t/`
should reach 10/10, and the two one-liners above should match raku.
