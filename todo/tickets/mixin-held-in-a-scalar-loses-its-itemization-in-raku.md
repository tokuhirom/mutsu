# A mixin held in a `$` loses its itemization marker in `.raku`

A container held in a `$` renders itemized — `.raku` prefixes it with `$`. Mixing
a role into that container drops the prefix:

```raku
role R { }
my $h = { a => 1, b => 2 };
say $h.raku;                            # raku: ${:a(1), :b(2)}   mutsu: ${:a(1), :b(2)}

my $mx = { a => 1, b => 2 } but R;
say $mx.raku;                           # raku: ${:a(1), :b(2)}   mutsu: {:a(1), :b(2)}
```

Only the rendering is wrong — the value itself behaves correctly, including
through subscripts (`$mx<a>:delete` removes the key and leaves
`{:b(2)}`/`${:b(2)}` respectively). So this is a `.raku` (and likely `.gist`)
concern in how a `Mixin` reports whether it is itemized, not a container bug: the
itemization that the `$` sigil confers is presumably read off the wrapped value's
variant, and the `Mixin` wrapper hides it.

Found while fixing `:delete` through a `$`-held container
([news](../../news/2026-08/delete-adverb-on-a-scalar-held-container.md)); it is
independent of that path and reproduces with no delete involved, as above.

Worth checking at the same time whether `but`-mixed **arrays** and quanthashes
lose the marker the same way, and whether `.item`/`.VAR` agree with `.raku` on a
mixin.
