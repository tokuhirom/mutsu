# Needle::Compile: role values, imported names in `MY::`, `.Failure`, topic calls

Working through Needle::Compile's test suite turned up five independent gaps.

- **`X but R<words>`** is rakudo's spelling of `X but R(<words>)` — the role
  plus its attribute's initial value — although `R<words>` on its own is just
  `Any`. mutsu evaluated the subscript and then failed to find a parametric
  candidate. The compiler now rewrites a `but`/`does` whose right side is a
  type name with a literal `<...>` key into the role-application call.
- **Imported types and terms in `MY::`.** `MY::`/`LEXICAL::` only listed the
  imported names still sitting in env under their import key, and then ran
  them through the filter that hides a module's incidental types from the
  global stashes. An imported class or role, a custom `sub EXPORT`'s types and
  constants, and an `is export` constant (whose key is the term key `\NAME`,
  displayed as `$\NAME`) were all missing. They are now read the way a bare
  word reads them, and a term shows under its bare name.
- **`Exception.Failure`** wraps an exception in an unhandled `Failure`, the
  value `Failure.new($exception)` builds — the `CATCH { return .Failure }`
  idiom. Both method-call opcodes handle it.
- **A role mixed into a string is still a `Str`.** Infix `~` and the string
  comparators now read its payload past any `Str`/`Stringy` the role declares,
  and interpolation honours only a declared `Stringy`, as rakudo does. A role
  `method Str { self ~ "" }` recursed forever before.
- **RakuAST construction:** `RakuAST::Call::Method.new`,
  `RakuAST::Term::TopicCall.new` (lowered to a method call on `$_`),
  `RakuAST::Term::Whatever.new`, and the `ArgList.args` accessor.

Needle::Compile still stops on RakuAST node mutation (`ArgList.push`,
`StatementList.unshift-statement`, `Str.AST(:compunit)`), which needs mutable
node identity and is filed as #11191.
