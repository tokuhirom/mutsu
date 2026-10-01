# CSS::Specification grammars: aliased interpolated subrules and eight related fixes

Working CSS::TagSet ([#10491](https://github.com/tokuhirom/mutsu/issues/10491)) exposed a chain
of interpreter gaps between a CSS stylesheet and its parsed, measured properties:

- **`<name={ code }>` and `<name=$var>`** now match the Regex the code returns (or the variable
  holds) as an anonymous subrule, filed under the alias only, with its own captures nested below
  it. CSS::Specification's `token val($*EXPR, $*USAGE='') { <proforma> || <rx={$*EXPR}> ||
  <usage($*USAGE)> }` used to fall through to the `usage` branch, whose action then read a `Nil`
  `$*USAGE`. `<name=$var>` also stopped capturing a second time under `$var`.
- **Token parameters** reach `<$x>`, `<name=$x>` and `<name={$x}>` in the token body: a Regex
  argument is bound in the env for the rule's match window, and the LTM measurement (which ignores
  arguments, ADR-0127) treats a body it cannot resolve without them as a fate instead of reporting
  `Variable '$x' is not declared`.
- **An inherited rule called with arguments** dispatches its subrules through the receiver
  grammar, like the argument-less path does (`grammar Top is A is B`: a rule of `A` sees `B`'s).
- **A type declaration in tail position** (`method build { my class builder is AST { … } }`) is
  the routine's, block's or `do`'s value, through one shared `compile_type_decl_value`.
- **`|$pair` with a non-Str key** is a named argument named by the key's string form, so
  `|(CSSObject::StyleSheet => …)` reaches `multi method load(:stylesheet!)`.
- **Composing a parametric role** no longer leaves its value parameters over a same-named
  variable of the calling routine (`role CSS::Units[\dimension, \units]` overwrote
  CSS::Properties' `:$units` default, so `reference-width` came out `Any`).
- **`my @a = … with $x`** (and `without`) declares the variable unconditionally and gates only the
  initializer, as `if`/`unless` already did.
- **A coercion type** leaves a value that already is its target alone, looking through an item
  container: `List()` given `$[1, 2]` is that Array, not a one-element list around it.
