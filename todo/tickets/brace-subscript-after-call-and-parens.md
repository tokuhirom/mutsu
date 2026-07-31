# Postcircumfix `{ }` not applied to sub-call results or parenthesized expressions

In Raku, an identifier or a closing paren followed **immediately** (no
whitespace) by `{ ... }` is a hash subscript on the value, not a block/hash
argument. mutsu mis-parses both shapes:

```raku
sub routes() { {a=>1} }
say routes{"a"};          # raku: 1
                          # mutsu: {a => Nil}   (treats {"a"} as a hash-composer argument)

my %h = b => 2;
say (%h // %h){"b"};      # raku: 2
                          # mutsu: prints %h and warns "Useless use of constant string"
                          #        ({"b"} becomes a separate sinked block)
```

When the sub has a non-zero arity the first shape dies with
`Too many positionals passed; expected 0 arguments but got more`. In statement
context inside a class body, the second shape escalates to a parse error
(`===SORRY!=== expected statement`), which is how it was found.

The `[ ]` postcircumfix is NOT affected: `(@a || @a)[0]` works.

## Impact (found by the 2026-07-31 web-framework survey)

- **Humming-Bird 4.1.0**: 5 of its 6 mutsu-attributable failing test files
  (`t/01,04,05,06,13`) die on `routes{'/'}{GET}` — this single fix should
  take the suite from 5/14 to ~10/14 (the raku baseline; 4 files fail under
  raku itself with a duplicate-import error).
- **Cro::HTTP::Router** (`lib/Cro/HTTP/Router.pm6:188`): fails to parse at
  `($!flattened-plugin-config // $!plugin-config){$key}.List` — this blocks
  `use Cro::HTTP::Router` entirely, i.e. the whole Cro server-side DX.

## Where

Parser postfix handling: after parsing a term (function call without parens,
or a parenthesized expression), a `{` with **no preceding whitespace** must
continue the postfix chain as a hash subscript, exactly like `[` does today.
Whitespace before `{` keeps the current block/hash-argument meaning.

Repro: `target/debug/mutsu -e 'sub routes() { {a=>1} }; say routes{"a"}'`
