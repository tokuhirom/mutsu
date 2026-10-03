# Six fixes from the PDF::Grammar test suite

PDF::Grammar's own tests went from 4 to 8 passing files out of 10, through
six general interpreter fixes:

- **String interpolation identifiers.** `"%PDF-{$v}"` interpolated a hash
  named `PDF-`; an identifier may contain `-` only between two word parts, so
  it is the literal `%PDF-` followed by a block.
- **`$.name:sym<x>(args)`.** A `.`-twigil variable's name is a longname, so
  the colonpair is part of the method name and the call reaches
  `method name:sym<x>`.
- **Loop sub-signatures.** `for @t -> % ( :$rule!, :$input, :$x = 'TOP',
  *%expected )` now binds `*%expected` to the named arguments the named
  parameters left, `*@rest` to every remaining positional, applies named
  defaults and enforces `!`.
- **A user method named like a trig function.** A class that declares its
  own `cos` (an actions class for a `token cos`) no longer has the call
  hijacked by the Numeric-coercion trig fallback.
- **CRLF against a negated character class.** `"\r\n"` is one grapheme that a
  class tests as `\n`, so `<-[\n]>` no longer matches it.
- **`:name[ X, ]` in a hash composer or named argument** keeps the trailing
  comma, so a lone hash stays one element instead of flattening into its
  pairs.

The two remaining files need enum values declared in a grammar body to stay
inside it (#11195).
