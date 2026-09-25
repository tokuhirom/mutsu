# Traits on the elements of a `my (...)` declarator list

A parenthesised declarator list is a signature in Raku, so each element may carry a
parameter trait:

```raku
my $x = 1; my $y = 2;
my ($a is rw, $b is rw) := ($x, $y);
$a = 5; say $x;   # 5
```

mutsu rejected this with `Confused. expected statement`, because the destructuring
declaration parser (`src/parser/stmt/decl/destructure.rs`) knew per-element types,
`where` clauses, defaults and `?`, but no traits. That was the first line
`CustomImporting` 0.0.5 died on in both of its test files
([#9320](https://github.com/tokuhirom/mutsu/issues/9320)).

The parser now accepts `is rw`, `is raw`, `is copy` and `is readonly` on a `$`/`@`/`%`
element, and refuses the rest the way rakudo does (an unknown trait, and `is rw` on an
`@`/`%` element, are compile-time errors with rakudo's wording). The traits decide how a
`:=` bind treats the element:

- `is rw` / `is raw` on a `$` element bind the staged element's container, exactly like a
  sigilless element or `my $a := EXPR` does (same `MarkBind` shape, same `__scalar_bind`
  marker). The alias writes through to the source, and an element with no container
  (`my ($a is rw) := (5,)`) stays immutable.
- `is copy` and `is readonly` leave the element the read-only copy it already was; rakudo
  does not give an `is copy` element of a declarator bind a writable container either.

In a list assignment (`my ($a is rw, $b) = 3, 4`) the traits are accepted and change
nothing, as in rakudo.

One gap remains, marked with a `TODO` at the site: rakudo refuses `is rw` against a
non-container at bind time (`X::Parameter::RW`), while mutsu only refuses the first write.

Pinned by `t/collections/list-destructuring-param-traits.t`.
