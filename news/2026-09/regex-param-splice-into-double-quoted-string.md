# A regex parameter spliced into a string is the value again, not its `.raku`

A `token`/`rule`/`regex` parameter reaches the pattern's `{ ... }` code blocks
by *textual* substitution: the resolution binds the arguments in a scratch
interpreter and then rewrites `$name` in the block's source with a literal of
the bound value, because the block itself runs at match time in the caller's
environment, where the parameter does not exist. The argument is rendered back
into the pattern the same way, as the subrule call's own argument text.

Both splices landed inside a **double-quoted Raku string** and escaped only the
backslash and the closing quote. So the spliced text was read back by the
enclosing string's own interpolation rules, and a value carrying any other
interpolation trigger did not survive:

```raku
grammar G {
    token TOP       { <inner('VALUE')> }
    token inner($x) { { say "plain=[$x]" } \w+ }
}
```

| `VALUE` | raku | mutsu (before) |
|---|---|---|
| `bar` | `plain=[bar]` | `plain=['bar']` |
| `a\b` | `plain=[a\b]` | `plain=['a\b']` |
| `a"b` | `plain=[a"b]` | *(nothing — the spliced `'a"b'` closed the string early)* |
| `a$b` | `plain=[a$b]` | `Use of Nil in string context` |
| `a{b}c` | `plain=[a{b}c]` | `plain=[abc]` |

The first two only leaked quotes. The last three are the ones that mattered:
the substituted text was re-read as Raku, so `$b` resolved against whatever the
*caller* happened to have in scope and `{b}` ran as a code block. Both produced
a wrong answer with no error at all — and `$b` did not have to be undeclared
for it to go wrong, it simply took that variable's value.

## What was actually wrong

Three distinct things, all the same mistake about quoting:

- **`bake_params_in_code_text` had no idea it was inside a string.** It tracked
  single quotes only, and substituted the same `value_to_raku_literal` form
  everywhere. A string is a different substitution position from code: `"[$x]"`
  wants the *value*, escaped so the string yields it, while `"[{$x}]"` — a code
  block nested in the string — wants the literal, and `'[$x]'` wants nothing at
  all. The scan now carries a context stack (code / single / double) rather
  than a pair of flags, with each code frame counting the brace depth it opened
  so the `}` that ends an interpolation block can be told from one that merely
  closes a nested block.
- **`format_named_regex_arg_value` rendered a `Str` argument as a
  double-quoted string escaping only `\` and `"`.** That text goes straight
  back into the pattern as `<rule("…")>` and is re-parsed, so the same
  characters were lost one layer earlier — before the parameter was ever bound.
  A debugger breakpoint on the baking entry point was what pinned this down:
  the value arriving there was *already* wrong, which ruled the baking pass out
  as the cause of that half.
- **Two code-block scanners counted braces without regard for quotes.** A
  brace inside a string literal (`{ say "a}b" }`) ended the block early, and
  the pass then rewrote the rest of the block as if it were pattern text.
  `find_matching_brace_end` was already quote-aware; its rule is now shared by
  a char-indexed spelling and used at both sites.

## Not fixed here

`<inner("a}b")>` — an *unmatched* closing brace inside a quoted subrule
argument — still fails, at parse time, from source, with no splice involved.
That is a different scanner and is filed as
[#8336](https://github.com/tokuhirom/mutsu/issues/8336).
