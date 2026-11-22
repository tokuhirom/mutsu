# An operator's code value keeps its core candidates beside user ones

```raku
sub call(&c, $a, $b) { c($a, $b) }
my &plain = &[cmp];
{ my &infix:<cmp> = -> $a, $b { "lexical" }; say call(&plain, 1, 2) }
class Foo { }
multi sub infix:<cmp>(Foo $a, Foo $b) { "foo" }
```

This died with "Type check failed in binding to parameter '$a'; expected Foo
but got Int (1)"; rakudo prints `Less` (#17631). With a user `multi` of a
built-in operator in the registry, `&[cmp]` is a dispatcher over the user
candidates. The caller's own lexical `&infix:<cmp>` kept that dispatcher from
redispatching by name. Its captured candidates then had no built-in to fall
back to, so the lone `Foo` candidate was bound to the Ints.

The dispatcher of a built-in operator's user candidates now falls back to the
operator, as a block-scoped import's dispatcher already did. A plain binary
operator's fallback runs the operator's own body: the user candidates have
just declined, and a lexical `&infix:<op>` of the calling scope is not the
operator the value stands for.
