# The "Odd number of elements" message shows an element container's value

When a hash initializer dies with `X::Hash::Store::OddNumber`, the message
names the element it stopped at by its `.raku`. When that element was an
element container (`my %c = (@a[0],)` after `@a[0] = %h`), mutsu stringified
the container itself and printed `Only saw: a	1`. rakudo prints
`Only saw: ${:a(1)}`. The renderer now dereferences the container first, so the
itemized hash shows as `${:a(1)}`, and the "Last element seen" form of a longer
list matches too (#11112).
