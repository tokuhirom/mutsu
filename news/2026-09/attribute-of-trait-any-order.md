# `of TYPE` may sit anywhere among an attribute's traits

[#9322](https://github.com/tokuhirom/mutsu/issues/9322), split out of the
[#7988](https://github.com/tokuhirom/mutsu/issues/7988) parse-gap cluster.
Shell::DSL's

```raku
has @!parts is required of Pipeable:D handles('elems', 'AT-POS', 'EXISTS-POS');
```

died with `===SORRY!=== Confused. Two terms in a row`, so the module could not
load.

`has_decl` already looped over the `is` / `will` / `does` / `handles` traits in
any order, but it parsed a postfix `of TYPE` only once, *after* that loop. An
`of` between two traits therefore ended the trait list, and the `handles` after
it was left over as a second term. `of` is now one more kind in the same loop,
so `is required of Int handles<elems>` parses like any other order. (A leading
`of`, before any `is`, still works as before.)

A second, smaller bug from the same issue: `X::Attribute::Required` always
named the attribute with a `$` sigil, so `has @!p is required` reported
`'$!p'`. All five places that raise it now use the attribute's own sigil
(`'@!p'`, `'%!h'`), matching rakudo.

Pinned by `t/oo/attribute/attribute-of-trait-order.t`.
