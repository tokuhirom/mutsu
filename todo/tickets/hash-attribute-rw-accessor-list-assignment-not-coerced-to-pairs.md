# Assigning a plain list through a `%`-sigil `rw` accessor doesn't coerce it to Hash pairs

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Mu.rakudoc:267`, with the
same root cause also visible in the `Type/Mu.rakudoc:238` raku-drift-bucketed example's `baz`
field).

## Repro

```raku
class Bar {
    has $.quux;
    has @.foo = <a b>;
    has %.bar = <a b c d>;
    method clone { nextwith :foo(@!foo.clone), :bar(%!bar.clone), |%_  }
}

my $o1 = Bar.new( :42quux );
with my $o2 = $o1.clone {
    .foo = <Z Y>;
    .bar = <Z Y X W>;
}

say $o1;
say $o2;
```

- raku:
  ```
  Bar.new(quux => 42, foo => ["a", "b"], bar => {:a("b"), :c("d")})
  Bar.new(quux => 42, foo => ["Z", "Y"], bar => {:X("W"), :Z("Y")})
  ```
- mutsu (`target/debug/mutsu`):
  ```
  Bar.new(quux => 42, foo => ["a", "b"], bar => {:a("b"), :c("d")})
  Bar.new(quux => 42, foo => ["Z", "Y"], bar => ("Z", "Y", "X", "W"))
  ```

`$o2.bar` should still be a `Hash` (the flat list `<Z Y X W>` coerced into key/value pairs, same
as a direct `%h = <Z Y X W>` assignment already does correctly). mutsu instead stores the raw list.

## Isolated minimal repro (no `clone`/no custom `.clone` method needed)

```raku
class Bar {
    has %.bar is rw = <a b c d>;
}
my $o = Bar.new;
say $o.bar;             # {a => b, c => d}     -- OK in mutsu too
$o.bar = <Z Y X W>;
say $o.bar;              # raku: {X => W, Z => Y}   mutsu: (Z Y X W)
```

A direct hash-variable assignment already coerces correctly:

```raku
my %h = <a b c d>;
%h = <Z Y X W>;
say %h;   # {X => W, Z => Y} -- correct in mutsu too
```

So the bug is specific to assignment routed through the auto-generated `rw` accessor *method*
for a `%`-sigil attribute — it doesn't go through the same list-to-hash coercion that a direct
`%var = list` assignment uses.

## Affected files (starting point)

- The auto-generated `is rw` accessor's assignment path for `%`-sigil attributes (likely near
  where `.=`/accessor-assignment compiles to a method call, or wherever the accessor's underlying
  container's assignment operator is invoked) — needs to route through the same hash-coercion
  logic used for direct `%var = list` assignment.
