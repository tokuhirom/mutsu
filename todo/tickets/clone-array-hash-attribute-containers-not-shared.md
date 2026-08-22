# Default `.clone()` doesn't share Array/Hash-typed attribute containers with the original

Found via direct verification while investigating the doc-diff harness's `Type/Mu.rakudoc:238`
finding (bucketed `raku-drift` by the harness because of an unrelated closure-gist text mismatch
in the same example — the harness's raku-drift bucketing for that finding is correct as far as
the closure text goes, but a second, real bug was hiding in the same example and is filed here
separately).

## Repro

```raku
class Foo {
    has $.foo is rw = 42;
    has @.bar       = <a b>;
    has %.baz       = <a b c d>;
}

my $o1 = Foo.new;
with my $o2 = $o1.clone {
    .foo = 70;
    .bar = <Z Y>;
}

say $o1;
say $o2;
```

- raku:
  ```
  Foo.new(foo => 42, bar => ["Z", "Y"], baz => {:a("b"), :c("d")})
  Foo.new(foo => 70, bar => ["Z", "Y"], baz => {:a("b"), :c("d")})
  ```
  (`$o1.bar` shows the *mutated* value `["Z", "Y"]` too — the default `.clone()` shares the
  `@.bar` Array container between `$o1` and `$o2`, so mutating it through either instance's
  accessor is visible on both, per the doc's own comment: "Hash and Array attribute modifications
  in clone appear in original as well".)
- mutsu (`target/debug/mutsu`):
  ```
  Foo.new(foo => 42, bar => ["a", "b"], baz => {:a("b"), :c("d")})
  Foo.new(foo => 70, bar => ["Z", "Y"], baz => {:a("b"), :c("d")})
  ```
  (`$o1.bar` still shows the original `["a", "b"]` — mutsu's `.clone()` gave `$o2` an independent
  copy of the `@.bar` array instead of sharing the container.)

## Analysis

Raku's default (Mu-inherited) `.clone()` does a *shallow* copy: scalar attributes get their own
fresh container, but reference-type attributes (Array, Hash) keep pointing at the *same*
underlying object as the original. Since `@.bar = <Z Y>` mutates the array container in place
(rather than rebinding the attribute to a new container), that mutation is visible through both
`$o1` and `$o2` when they share the container. mutsu's `.clone()` appears to deep-copy Array/Hash
attributes instead, breaking that sharing.

## Related

This may share a root cause with the already-filed
[container-aliasing-not-preserved-into-collection.md](container-aliasing-not-preserved-into-collection.md)
ticket (mutsu snapshotting containers by value where raku aliases them) — that ticket covers the
`.push`/collection-insertion case, this one covers the `.clone()` attribute-copy case. Worth
checking together, but filed separately since `.clone()`'s copy semantics are a distinct code
path from array `.push`.

## Affected files (starting point)

- The built-in `.clone()` implementation for instances with Array/Hash-typed attributes — find
  where it iterates attributes and copies each one; it needs to preserve container identity
  (share the same `Gc<Array>`/`Gc<Hash>` reference) for reference-type attributes instead of
  deep-copying them.
