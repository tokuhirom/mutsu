# Five more parse-failure index rows: parameter tails, negated set glyphs, subscript semilists

Five more constructs off the `expected statement ...` index (#7954), the largest
single bucket in the ecosystem sweep's `blocked_load` records. Each was reached
the same way the earlier batches were: fetch the tarball named by the
`ecosystem/dists/` record, run `mutsu --dump-ast` over every module the META6
`provides` names, binary-search the failing file down to the smallest still-failing
line range, and reduce that to a one-liner checked against rakudo.

Four of the five are the same shape of bug — a parameter (or subscript) grammar
that stopped one production short of what Raku allows, leaving the tail
unconsumed so the *enclosing* construct failed at its closing bracket. That is
why the index could only ever report a container: the reported line named the
`sub`, the `class` or the block, never the parameter inside it.

## A parameter trait written with a word-quote argument

`sub MAIN(Bool :$timer is option<!>)` — App::Prove6's entire CLI signature, via
`Trait::Option`. Parameter traits accepted only a parenthesized argument
(`is encoded('utf8')`), so the `<!>` was left in the signature and the parameter
list failed at its `)`. `skip_optional_trait_arg` now also skips a `<...>` /
`«...»` word quote written directly after the trait name. The opener has to
follow the name with no intervening whitespace, which keeps
`$x is copy where * < 3` out of this path.

## An anonymous optional parameter carrying a tail

`multi sub build-xxhash(Int @data, Int $seed = 0, $? where { $*KERNEL.bits == 64 } --> Int)`
— Digest::xxHash. The `$?` / `@?` / `%?` branch returned the parameter the moment
it recognized the sigil, so an `is` trait, a `where` post-constraint or a default
after it was never consumed. It now runs the same `parse_subsig_tail` every other
parameter shape uses. An anonymous optional *invocant* (`method m($?: |)`) carries
none of those, so it still falls through untouched.

## The `!` meta-prefix over a Unicode set relation

`@vars .= grep: * !∈ @$positional` — Math::Symbolic. The negated-set-operator
parser listed the ASCII spellings (`!(elem)`, `!(<=)`, ...) by hand and knew only
the four *precomposed* negated glyphs (`∉`, `∌`, `⊈`, `⊉`), so `!` followed by a
glyph was not an operator at all. It now defers to `parse_set_op` and keeps
whatever that returns when the relation is Bool-valued, which is the rule the `!`
meta-prefix actually follows. The non-Bool set operators stay out, so `!∪` is
still not an operator.

## A sigilless pointy-block parameter carrying a trait

`Proxy.new: :FETCH{ ... }, STORE => -> $, \v is raw { ... }` — Proxee. The
sigilled and `+name` pointy-parameter branches each parsed an `is`-trait and
default tail; the `\name` branch returned immediately, so the trait was left
before the block's `{` and the whole `Proxy.new` argument list failed.

## A semicolon terminating a subscript's semilist

```raku
@!kept[
    $!nb-kept < $!nb-to-keep ?? $!nb-kept++ !! $!nb-to-keep.rand;
] = [$!nb-seen, $item];
```

Data::RandomKeep. A subscript holds a semilist, so a `;` in it may *terminate*
the last dimension rather than separate two — `@a[1;]` is `@a[1]`, not a
two-dimensional index whose second dimension is missing. The dimension loop
unconditionally parsed another expression after every `;`, so the statement died
at the closing bracket. It now peeks for the subscript's own closer (`]` or `}`)
first, and a single remaining dimension collapses back to an ordinary
`Expr::Index` rather than a one-dimensional `MultiDimIndex`.

## Result

App::Prove6, Data::RandomKeep, Digest::xxHash and Proxee now parse every module
their META6 `provides` names; so does Math::Symbolic's own `Math/Symbolic.rakumod`
(`Language.rakumod` still fails on `our @.operations := @operations;`, an `our`-scoped
attribute bound with `:=`, which remains on the index).

Pins: `t/oo/trait/param-trait-word-quote-arg.t`,
`t/routines/signature/anon-optional-param-tail.t`,
`t/collections/set-bag-mix/negated-set-op-unicode-glyphs.t`,
`t/routines/signature/pointy-sigilless-param-traits.t`,
`t/collections/subscript/subscript-semilist-trailing-semicolon.t` — all five green
under rakudo itself, so they pin rakudo's behaviour rather than mutsu's.

One divergence found on the way and filed as #8089 rather than forced in here: an
*optional* parameter's `where` constraint is skipped entirely when the argument is
omitted, where rakudo runs it against the type object. That is what makes
Digest::xxHash's `$? where { $*KERNEL.bits == 64 }` a real multi-dispatch
discriminator, and it reaches well beyond this index — it lives in the binder and
in multi-candidate matching, not in the parser.
