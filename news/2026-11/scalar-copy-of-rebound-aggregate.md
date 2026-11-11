# A scalar holding a closure-rebound hash keeps it across the rebind

`my $alias = %state` puts the Hash in `$alias`'s Scalar. mutsu shares an
aggregate with a scalar through a cell (`promote_array_share_source`), and for
a variable that a closure rebinds with `:=` the slot holds a *binding cell*
whose content is that aggregate cell (ADR-12529 §3). The share took the binding
cell itself, and re-seated the source's slot with it, so `$alias` followed
every later `%state := ...`:

```raku
my %state = count => 0;
my $alias = %state;
my &ws = -> $_ { %state := $_ };
ws({count => 9});
say $alias<count>;    # 0; mutsu printed 9
```

The share now takes the aggregate cell inside the binding and leaves the
source's binding as it is (#17482). Element writes through either name still
reach the shared Hash until the rebind.
