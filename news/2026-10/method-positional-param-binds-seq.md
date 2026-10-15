# A Positional-typed method parameter binds a Seq

`method m(Positional $p)` (and `multi method` candidates typed `Positional`) now
accept a `Seq` the way `sub f(Positional $p)` already did (rakudo#4864): the Seq
is bound through `PositionalBindFailover` and arrives as a `List`. The fast
method binder used to reject it with "Type check failed in binding to
parameter '$node'; expected Positional but got Seq".

Found while running the Pod::To::HTML distribution: `Node::To::HTML.node2html`
receives `$pod.map: { ... }`. Eight more of its test files (`040-lists`,
`070-headings`, `075-defn`, `080-lang`, `090-css`, `110-issue-41`,
`111-perl6-doc-issue-2270`, `140-config`) now match rakudo.
