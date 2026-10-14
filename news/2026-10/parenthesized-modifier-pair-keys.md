# Modifier keywords remain pair keys after list commas

Parenthesized and bracketed lists now distinguish a statement modifier from a
bareword pair key followed by `=>`. A multiline record containing `given => ...`,
as used by BDD::Behave::Runner, no longer tries to parse the pair value as a
`given` modifier. Inline modifiers share the existing trailing-comma predicate,
so ordinary conditional list modifiers retain their behavior.
