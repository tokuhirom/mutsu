# An untaken `with %h<k>` no longer steals the next topic's writeback

`with X { }` topicalizes the value of `X`. When `X` is a container
element (`%h<k>`, `@a[i]`), the compiler tags that element, and the
topicalizing `given` then writes `$_` back into it. That tag is a one-shot
signal: the `given` consumes it. When the element is undefined, the `given`
never runs, so the compiler emits a `ClearElementSource` to drop the tag.
For a `with` with no `else`, that clear was emitted on the *taken* path,
where the tag was already gone, and never on the false path.

The pending tag then hijacked the next topicalizing construct anywhere in
the run. `my %g; with %g<a> { }; my $y; $_ = 5 without $y` left `$y`
undefined and stored `5` into `%g<a>` instead (raku: `$y` is 5 and `%g`
stays empty).

PDF::Font::Loader::CSS hit this. Its test calls `URI.new("./")`, and
URI's parser runs `with $comp_container<authority> -> $auth { ... }` on a
relative reference, which has no authority. Later, CSS::Font's
`$_ .= new(|c) without $!css` lost its writeback, so `$!css` stayed a type
object, and `"bold italic 12pt DejaVu Sans"` could not coerce to
`CSS::Font:D()`. The false path now clears the tag. The test
`t/control/without-topic-writeback-after-untaken-with.t` pins this.
