# ASN::Grammar parses: sigspace quantifiers, proto candidate calls, LTM fates

`ASN::Grammar` failed to parse its LDAP specification. With three regex fixes
its test file passes 8/8.

- **A quantifier that significant whitespace follows backtracks under
  sigspace.** Rakudo ratchets the `[atom <.ws>]` wrapper there, not the
  quantifier. So `rule { 'DEFAULT' <id-string>? <value> }` hands `FALSE` back
  from `<id-string>` to `<value>`. An explicit `:` still commits, and a
  quantifier written directly against what follows (`<id>?<v>`) stays
  ratcheted. The whitespace between `**` and its count (`a ** 1..3`) is now
  part of the quantifier rather than an inserted `<.ws>`.
- **`<value:sym<number>>` calls that one proto candidate**, captured under its
  long name, instead of the whole proto. The alternation splitter no longer
  reads the closing `>>` as a word boundary, which had hidden every `|` after
  such a call.
- **An undeclared subrule in a `|` branch is an LTM fate.** As in Rakudo's NFA,
  the branch ranks by what precedes it, so a longer declared sibling wins and
  the missing method is only reported when a real match calls it.
