# A `$`-held Seq stays one element in an `@` container

A Seq held in a Scalar is one item when it is stored into an Array, exactly as an itemized List
or Array is. mutsu flattened it in three places:

```raku
class G { has @.a }
my $s = (1, 2).map({ $_ });
say G.new(a => $s).a;            # was [1 2], now [(1 2)]
my @f; @f.append($s); say @f;    # was [1 2], now [(1 2)]
```

The array coercion behind `@`-attribute construction and `@`-container assignment
(`coerce_finite_to_array`) and the one-argument rule of `append`/`prepend`
(`flatten_append_args`) now treat a Seq in its `$` view (`ItemSeq`/`ItemList`) as a single
element, as they already did for an itemized Array. Destructuring (`my ($a, $b) = $seq`) and
`splice` still iterate it, as in Rakudo (#18193).
