# Math::NIntegrate loads and its suite almost passes

Math::NIntegrate was `blocked_load`: its grammar module failed to parse.
Fixing that exposed several more gaps:

- **A regex literal inside a regex code block.** In `<!{ $0.Str ~~ / 'x'
  <["']>? $/ }>`, the quote characters in the nested regex's character class
  were read as string openers. The enclosing rule then never closed ("Missing
  block"). Both the parser's delimiter scan and the runtime's code-block scan
  now treat a `/` in term position as the start of a nested regex literal and
  skip it regex-aware (`src/regex_code_nested.rs`).
- **`@x».&f <<*>> @w`.** The dynamic hyper method call consumed the
  whitespace after `&f` even without an argument list, so `<<*>>` was read as
  a `<<...>>` subscript.
- **`self.attr = |@v`.** A Slip assigned to a method lvalue flattened into the
  internal call's arguments, and only its first element was stored.
- **`:&!code` attributive parameters.** They were marked readonly like
  ordinary parameters. Separately, a `&!g = ...` that ends a `given`/`with`
  body was stored under `&!g` instead of the attribute's `!g` key.
- **`cross()`.** A one-element argument holding a list (`[(0, 1),]`) was
  unwrapped into the list's elements. Each argument is now iterated as-is,
  and the single-argument rule applies only when one argument is passed.
- **Hyper operands that are element containers.** `(1,2,3) <<*>> @a.head`
  hypered the element container as a scalar (its `.elems`) instead of
  element-wise.

As a result, 17 of the 18 test files pass under mutsu. The last subtest of
`t/22` needs #11391: `try`'s implicit fatal mode leaks into the routines it
calls.
