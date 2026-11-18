# The `.&` call forms keep their `&` in `.AST` and `.DEPARSE`

`"ab".&({ ... })` and `5.&($f)` (a parenthesized callee) deparsed as `"ab".({ ... })`, and the topic
forms `.&foo`, `.&foo(3)` and `.&{ ... }` as `&foo($_)` / `({ ... })($_)`, because the parser turns a
topic call into a call with `$_` first. Rakudo itemizes a block or parenthesized callee
(`Contextualizer::Item`) and keeps the topic form as `Term::TopicCall(<dynamic call>)`. The view now
builds those; lowering takes the callee back out of the item and the topic call back into the call on
`$_`, so `EVAL($ast)` is unchanged.
