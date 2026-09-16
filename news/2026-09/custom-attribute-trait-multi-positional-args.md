# A custom attribute trait's parenthesized call keeps multiple positional args

`is traitname(a, b)` on a `has` declaration silently dropped every positional
argument once there was more than one. The parser handed the whole
parenthesized span of an unknown lowercase `is` trait (the case dispatched to
a user-defined `trait_mod:<is>` at class registration) to a single-expression
parser. For a single argument (`is doc('barks')`) that worked fine, but for
two or more the parse always left a leftover comma after the first item, so
the whole attempt was treated as a failure and the trait's argument silently
fell back to the "no argument" case — passing `Bool::True` instead of the
real values.

Raku itemizes a multi-value trait call's arguments into one `List` when the
trait sub's own parameter is a scalar named parameter: `is xml-namespace('urn',
'prefix')` against `multi trait_mod:<is>(Attribute:D $attr, :$xml-namespace!)`
sees `$("urn", "prefix")`, not just the first value or `True`.

`parse_trait_call_args` (`src/parser/stmt/decl/has_decl.rs`) now parses the
parenthesized content as a full comma-separated expression list: a single
item keeps its own `Expr` (unchanged behavior), and two or more become an
`Expr::ArrayLiteral`, matching Raku's itemization.

Reduced from XML::Class 0.0.11's `is xml-namespace('urn', 'prefix')`
([#8528](https://github.com/tokuhirom/mutsu/issues/8528)), which used this to
bind a `Bool` where a `Str` was required in its `t/030-namespace-out.t` and
`t/070-namespace-in.t`. Pinned by
`t/oo/attribute/attr-trait-multi-positional-args.t`. The other five bullets
in #8528 (multi-dispatch ambiguity, typed-local reinitialization, nested type
construction, and the rest) are separate, unrelated interpreter gaps and
remain open.
