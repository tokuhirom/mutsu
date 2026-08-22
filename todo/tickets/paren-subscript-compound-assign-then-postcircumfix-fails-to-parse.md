# A parenthesized hash-subscript compound-assignment followed by a chained postcircumfix `{}` fails to parse

Discovered while evaluating the `LogP6` logging module (the runner-up candidate to `Log::Async`,
which is already the chosen battery) for possible bundling. This is a niche parser gap unrelated
to `Log::Async` itself; it only came up because `LogP6`'s own source uses this idiom internally.

## Root cause

mutsu's parser fails to chain a postcircumfix `{...}` subscript onto a parenthesized expression
when that expression's last (or only) term is a **hash-subscript compound assignment**
(`%h{"key"} //= value`, and likewise `+=`, `||=`, etc. — plain scalar compound assignment like
`($x //= value){...}` parses fine, so the trigger is specifically the postcircumfix `%h{"key"}`
as the LHS of the compound-assign, not compound assignment in general).

Two symptoms were observed depending on whether the paren+subscript sits at statement level or as
a call argument:

1. At statement level, writing to the chained subscript is a hard parse error:
   ```
   ===SORRY!=== Error while compiling ...
   Confused. expected statement: expected use statement or import statement or no statement or need statement or unit statement or ...
   at ...:2
   ------>(%h{"a"} //= "x"){"k"} = 1;
                                 ^
   ```
2. As a `say` argument, mutsu does not error, but silently mis-parses: it treats `(%h{"a"} //= "x")`
   as the complete argument to `say`, then treats the trailing `{"k"}` as a **separate block
   statement** (a bare block containing the string literal `"k"`), producing a bogus
   "Useless use of constant string ... in sink context" warning and printing the wrong value.

Likely relevant parser code: the postfix/postcircumfix chaining logic in
`src/parser/expr/operators.rs` (which handles postfix operator chaining including subscripts)
and/or the primary-expression handling of parenthesized terms under `src/parser/primary/`. The
investigation did not go further than isolating the trigger condition (no `rust-gdb`/AST-dump
deep dive was done, per the scope of this evaluation task) — a full root-cause fix will need to
trace how the parser resumes postfix-chain parsing after closing a compound-assignment expression
nested inside parens.

## Affected files

- `src/parser/expr/operators.rs` (postfix/postcircumfix chaining — best current guess)
- `src/parser/primary/` (parenthesized-term parsing)
- Exercised indirectly by `~/.zef`-style third-party code such as `LogP6::create-and-store-loggers`
  (`lib/LogP6.rakumod`), which contains the exact pattern:
  ```raku
  (%cliches-to-traits{$cliche.name} //= SetHash.new){$trait} = True;
  ```

## Repro

Minimal repro (verified: raku accepts and runs both; mutsu fails to parse both), against
`target/release/mutsu` built from this repo at commit `5ca0dc45b` (2026-08-22):

```raku
my %h;
(%h{"a"} //= "x"){"k"} = 1;
```

```
$ raku /tmp/tO.raku   # (no output; runs fine)
$ target/release/mutsu /tmp/tO.raku
===SORRY!=== Error while compiling ...
Confused. expected statement: expected use statement or import statement or no statement or need statement or unit statement or ...
at ...:2
------>(%h{"a"} //= "x"){"k"} = 1;
                              ^
```

The `say`-argument variant (silent mis-parse instead of a hard error):

```raku
my %h;
say (%h{"a"} //= "x"){"k"};
```

```
$ raku /tmp/tN.raku
Type Str does not support associative indexing.
  in block <unit> at /tmp/tN.raku line 2
# (raku errors too, but for a different, expected reason: "x"{"k"} is a Str
#  subscripted associatively, which is a legitimate runtime type error, not a parse error)

$ target/release/mutsu /tmp/tN.raku
Useless use of constant string "k" in sink context
    at /tmp/tN.raku:2
x
# mutsu instead parses `{"k"}` as a disconnected bare block statement.
```

Also confirmed: replacing `//=` with `+=` or `||=` on the same `%h{"a"} OP= EXPR` shape reproduces
the statement-level parse error identically. Replacing the hash-subscript LHS with a plain scalar
(`($x //= 1){"k"} = 1;`) parses and runs correctly in mutsu, which pins the trigger to the
combination of "postcircumfix hash-subscript as compound-assignment target, inside parens,
followed by another postcircumfix subscript."

## Why this is a separate ticket

This is unrelated to `Log::Async` (the chosen logging battery) and unrelated to any currently
bundled module; it surfaced purely from probing `LogP6` as a runner-up candidate during the
logging-module bundling evaluation. `LogP6` itself was not otherwise pursued further once this
parser gap was found, since fixing it is out of scope for that evaluation task.
