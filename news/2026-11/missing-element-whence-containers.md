# A Block's tail on a missing element is the element's container

A Block whose tail subscripts a missing key or index now hands back that
element's container, as Rakudo does: binding it creates nothing, and the first
write through it vivifies the entry.

```raku
my %h; my $c := (-> { %h<a> })(); $c = 5; say %h.raku;   # {:a(5)}
my $x; my $c2 := $x<a>; $c2 = 1; say $x.raku;             # ${:a(1)}
```

The block tail kept the deferred entry token only when the block was called
as an assignment target, and otherwise read it as `Any`. It now always hands
the token back (it reads as `Any` wherever a value is wanted), reads a plain
variable receiver raw so a token or the cell of an undefined Scalar extends the
location, and a subscript bound on a Scalar that still holds `Any` roots its
deferred path at that Scalar's cell (Rakudo's `Any.AT-KEY` whence container).

That retires `Interpreter::hash_autovivify`, the dynamically scoped flag
`reduce` set while it called an `is raw` callback so that every hash read under
it created missing keys eagerly: `(%hash, |<a b c>).reduce: -> $h is raw, $k {
$h{$k} }` now descends through the tails' containers (#17953, ADR-10779 D3).
