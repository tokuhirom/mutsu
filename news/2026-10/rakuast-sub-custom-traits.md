# RakuAST: a sub's custom `is` traits, and slurpy pointy parameters

"sub with traits / multi / export" was the most common `.AST` refusal, with
67 `t/` files. The refusal now names the part it could not render. Custom
traits accounted for 51 of the files: `is native` 34 times, then
`is test-assertion`, `is default`, `is cached` and user `trait_mod:<is>`
traits. Precedence traits accounted for 10 files.

Measured on rakudo 2026.09, `sub f() returns Str is native("libc") { * }`
carries `Trait::Returns` and then
`Trait::Is(name => "native", argument => Circumfix::Parentheses(SemiList(…)))`.
mutsu's parser keeps custom traits in `custom_traits` in source order,
together with the `returns`/`of` marker. So the converter renders each one as
a `Trait::Is` in that order around the return-type trait, and the lowering
rebuilds the same list. A list argument `(1, 2)` is the parser's
`Grouped(ArrayLiteral)`, and its parentheses are the circumfix.
`is test-assertion` also sets the sub's `is_test_assertion` flag again.

Some cases stay refused:

- a multi-word angle argument (`is foo<a b>`);
- a parser-internal marker or a qualified trait name;
- a custom trait beside `is rw`, `is raw` or `is export`, since the parser
  does not keep where they were written.

A single-word angle argument comes back as `('x')`, which means the same.

Testing the round trip turned up two parameter lowerings that changed what a
program did under `MUTSU_RAKUAST=1`:

- A pointy block whose one parameter is slurpy (`-> |c { … }`,
  `-> *@a { … }`) collapsed into a plain one-parameter lambda, so it rejected
  any call that did not pass exactly one argument. That broke every
  `&r.wrap(-> |c { callsame })`.
- `**@a` came back double-slurpy but not slurpy, a combination the parser
  never builds.
