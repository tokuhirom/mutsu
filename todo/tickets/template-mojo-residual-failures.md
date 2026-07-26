# Template::Mojo: three residual failures after the quoted-angle regex fix

Split out of `todo/tickets/grammar-named-capture-resolved-as-method.md` on
2026-07-26. That ticket's root cause — a quoted `<`/`>` inside a regex assertion
breaking the parse — is fixed (`news/2026-07/regex-assertion-quoted-angle-brackets.md`),
taking `Template::Mojo` 0.2.2 from every test file dying immediately to:

| file | mutsu | raku |
| --- | --- | --- |
| `t/00-basic.rakutest` | 15/17 | 17/17 |
| `t/01-template.rakutest` | 3/3 | 3/3 |
| `t/02-complex.rakutest` | 1/1 | 1/1 |
| `t/03-capture.rakutest` | 0/1 | 1/1 |
| `t/04-native-named.rakutest` | 1/1 | 1/1 |

Reproduce by unpacking the dist and running `mutsu -I lib t/<file>` (the tarball
URL is in the git history of the original ticket).

## 1. `00-basic` tests 16 & 17 — arity error message from EVAL'd code

```raku
Template::Mojo.new('<%= $^a + $^b %>').render(23);
# the test asserts the caught error matches /expected\s2/ and /got\s1/
```

The template compiles to a `sub` **built as source text and EVAL'd**, whose body
uses `$^a`/`$^b`. Called with the wrong number of arguments, raku's message is
`Too few positionals passed; expected 2 arguments but got 1`.

Note that mutsu already produces exactly that message for a plain placeholder
block (`my $f = { $^a + $^b }; $f(23)` — byte-identical to raku), so the gap is
specific to the EVAL'd `sub NAME { ... $^a ... }` shape the module builds, or to
how the error surfaces through `.render`. Start by reducing
`EVAL 'sub t { $^a + $^b }'` and calling it with one argument.

## 2. `03-capture` — `Use of Nil in string context` in the action

```
Use of Nil in string context
  in sub expr at lib/Template/Mojo.rakumod line 72
```

`method perlline($/) { make expr($/) ~ "\n" }` — the helper `expr` reads
`$<get-result>` / `$<expr>` off the match. One of them is `Nil` where raku has a
value, for the `% my $x = ...` capture-block template this file exercises. Since
the surrounding grammar now parses, this is a capture-population difference on
the `perlline` token (`^^ \h* '%' $<get-result>=['=']? $<expr>=[ <-[\n]>* ] [\n | $]`)
— most likely the optional `$<get-result>=['=']?` capture when it does not match.

## Why they are separate

Both are independent of the angle-bracket parse bug and of each other, and
neither is a grammar-scanner issue. The first is placeholder/arity plumbing
through EVAL; the second is capture population for an optional capture.
